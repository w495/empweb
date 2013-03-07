-module(empweb_biz_session).
-behaviour(gen_server).

-vsn("3").


-export([
    start_link/0,
    new/1,
    get/1,
    gen_uid/1,
    remove/1,
    timeout/0
]).


%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).


-include("empweb_biz_session.hrl").
-include_lib("stdlib/include/qlc.hrl").


-define(EMPWEB_BIZ_SESSION_TIMEOUT, 300000). %% 5*60*1000

-define(EMPWEB_BIZ_SESSION_EXPIRETIMEOUT, 1800000). 
%%
%% see also empweb_http
%%


-define(EMPWEB_BIZ_SESSION_TABLENAME,
    list_to_atom(atom_to_list(node()) ++ ".biz_session")
).


%% ====================================================================
%% External functions
%% ====================================================================

start_link()->
    timer:apply_interval(
        300000,
        ?MODULE,
        timeout,
        []
    ),
    amnesia:start_link([
        {local_tables, [
            {?EMPWEB_BIZ_SESSION_TABLENAME, [
                {disc_copies, [node()]},
                {local_content, true},
                {record_name, empweb_biz_session},
                {attributes, record_info(fields, empweb_biz_session)}
            ]}
        ]}
    ]).

gen_uid(Login) ->
    Uid = <<(empweb_uuid:to_string(empweb_uuid:v4()))/binary,(empweb_convert:to_binary(Login))/binary>>,
    case ?MODULE:get(Uid) of
        [] -> Uid;
        _ -> gen_uid(Login)
    end.

new(#empweb_biz_session{uid=undefined} = Biz_session) ->
    new(Biz_session#empweb_biz_session{
        uid = gen_uid(Biz_session#empweb_biz_session.id)
    });

new(#empweb_biz_session{uid=Uid, id=Id} = Biz_session) ->
    remove_dubles(Id),
    amnesia:write(
        ?EMPWEB_BIZ_SESSION_TABLENAME,
        Biz_session#empweb_biz_session{
            uid = Uid,
            time=erlang:universaltime()
        },
        write
    ),
    Uid.

get({uid, Uid}) ->
    amnesia:read({?EMPWEB_BIZ_SESSION_TABLENAME, Uid});

get(Uid) ->
    amnesia:read({?EMPWEB_BIZ_SESSION_TABLENAME, Uid}).

remove({login, Uid}) ->
    amnesia:delete({?EMPWEB_BIZ_SESSION_TABLENAME, Uid});

remove({uid, Uid}) ->
    amnesia:delete({?EMPWEB_BIZ_SESSION_TABLENAME, Uid});

remove(Uid) ->
    amnesia:delete({?EMPWEB_BIZ_SESSION_TABLENAME, Uid}).

seconds(Item) ->
    calendar:datetime_to_gregorian_seconds(Item#empweb_biz_session.time).

id(Item) ->
    Item#empweb_biz_session.id.

expired(Curtime, Item) ->
    Curtime - seconds(Item) > ?EMPWEB_BIZ_SESSION_EXPIRETIMEOUT.

eqid(Id, Item) ->
    Id == Item#empweb_biz_session.id.

remove_expired() ->
    Curtime = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    Function =
        fun() ->
            Query =
                qlc:q([
                    Item
                    || Item <-
                        amnesia:table(?EMPWEB_BIZ_SESSION_TABLENAME),
                        expired(Curtime, Item)
                ]),
            Set = qlc:e(Query),
            lists:foreach(
                fun(#empweb_biz_session{uid = Uid}) ->
                    amnesia:delete(?EMPWEB_BIZ_SESSION_TABLENAME, Uid)
                end,
                Set
            )
        end,
    amnesia:transaction(Function).

remove_dubles(Id) ->
    Function =
        fun() ->
            Query =
                qlc:q([
                    Item || Item
                    <- amnesia:table(?EMPWEB_BIZ_SESSION_TABLENAME),
                    eqid(Id, Item)
                ]),
            Set = qlc:e(Query),
            case  erlang:length(Set) > 10 of
                true ->
                    {Ruid, _} = lists:foldl(
                        fun(#empweb_biz_session{'time' = Time, uid = Uid}, {Auid, Atime}) ->
                            Ctime = calendar:datetime_to_gregorian_seconds(Time),
                            case Ctime < Atime of
                                true ->
                                    {Uid, Ctime};
                                _ ->
                                    {Auid, Atime}
                            end
                        end,
                        {[], calendar:datetime_to_gregorian_seconds(erlang:universaltime())},
                        Set
                    ),
                    amnesia:delete(?EMPWEB_BIZ_SESSION_TABLENAME, Ruid);
                false ->
                    ok
            end
        end,
    amnesia:transaction(Function).

timeout()->
    remove_expired().


%%%
%%% ===========================================================================
%%%




%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: []
%% --------------------------------------------------------------------


init([]) ->
    {ok, []}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------


handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State) ->
    {noreply, State};

handle_info(_sInfo, State) ->
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Local
%% --------------------------------------------------------------------
