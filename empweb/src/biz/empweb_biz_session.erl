-module(empweb_biz_session).


-export([
    start/0,
    new/1,
    get/1,
    gen_uid/1,
    remove/1,
    timeout/0
]).


-export([
    read/1,
    write/3,
    delete/1,
    transaction/1
]).

-include("empweb_biz_session.hrl").
-include_lib("stdlib/include/qlc.hrl").

start() ->
    %%% Зачем нам нужна амнезия ?
    empweb_biz_session_amnesia:start().


gen_uid(Login) ->
    Uid = <<(empweb_uuid:to_string(empweb_uuid:v4()))/binary,(empweb_convert:to_binary(Login))/binary>>,
    case ?MODULE:get(Uid) of
        [] -> Uid;
        _ -> gen_uid(Login)
    end.

new(#biz_session{uid=undefined} = Biz_session) ->
    ?MODULE:new(Biz_session#biz_session{
        uid = gen_uid(Biz_session#biz_session.id)
    });

new(#biz_session{uid=Uid, id=Id} = Biz_session) ->
    remove_dubles(Id),
    write(
        ?EMPWEB_BIZ_SESSION_TABLENAME,
        Biz_session#biz_session{
            uid = Uid,
            time=erlang:localtime()
        },
        write
    ),
    Uid.

get({uid, Uid}) ->
    ?MODULE:read({?EMPWEB_BIZ_SESSION_TABLENAME, Uid});

get(Uid) ->
    ?MODULE:read({?EMPWEB_BIZ_SESSION_TABLENAME, Uid}).

remove({login, Uid}) ->
    ?MODULE:delete({?EMPWEB_BIZ_SESSION_TABLENAME, Uid});

remove({uid, Uid}) ->
    ?MODULE:delete({?EMPWEB_BIZ_SESSION_TABLENAME, Uid});

remove(Uid) ->
    ?MODULE:delete({?EMPWEB_BIZ_SESSION_TABLENAME, Uid}).

seconds(Item) ->
    calendar:datetime_to_gregorian_seconds(Item#biz_session.time).

id(Item) ->
    Item#biz_session.id.

expired(Curtime, Item) ->
    Curtime - seconds(Item) > ?EMPWEB_BIZ_SESSION_EXPIRETIMEOUT.

eqid(Id, Item) ->
    Id == Item#biz_session.id.

remove_expired() ->
    Curtime = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    Function =
        fun() ->
            Query =
                qlc:q([
                    Item
                    || Item <-
                        mnesia:table(?EMPWEB_BIZ_SESSION_TABLENAME),
                        expired(Curtime, Item)
                ]),
            Set = qlc:e(Query),
            lists:foreach(
                fun(#biz_session{uid = Uid}) ->
                    mnesia:delete({?EMPWEB_BIZ_SESSION_TABLENAME, Uid})
                end,
                Set
            )
        end,
    ?MODULE:transaction(Function).

remove_dubles(Id) ->
    Function =
        fun() ->
            Query =
                qlc:q([
                    Item || Item
                    <- mnesia:table(?EMPWEB_BIZ_SESSION_TABLENAME),
                    eqid(Id, Item)
                ]),
            Set = qlc:e(Query),
            case  erlang:length(Set) > 10 of
                true ->
                    {Ruid, _} = lists:foldl(
                        fun(#biz_session{'time' = Time, uid = Uid}, {Auid, Atime}) ->
                            Ctime = calendar:datetime_to_gregorian_seconds(Time),
                            case Ctime < Atime of
                                true ->
                                    {Uid, Ctime};
                                _ ->
                                    {Auid, Atime}
                            end
                        end,
                        {[], calendar:datetime_to_gregorian_seconds(erlang:localtime())},
                        Set
                    ),
                    mnesia:delete({?EMPWEB_BIZ_SESSION_TABLENAME, Ruid});
                false ->
                    ok
            end
        end,
    ?MODULE:transaction(Function).

timeout()->
    remove_expired().


%%%
%%% ===========================================================================
%%%


transaction(F) ->
    case mnesia:transaction(F) of
        {atomic, Result} ->
            Result;
        {aborted, _Reason} ->
            []
    end.

write(Table, Rec, Mod) ->
    transaction(fun() ->
        mnesia:write(Table, Rec, Mod)
    end).

delete(Oid) ->
    transaction(fun() ->
            mnesia:delete(Oid)
    end).

read(Oid) ->
    transaction(fun() ->
        mnesia:read(Oid)
    end).

%
% find(Q) ->
%     transaction(fun() ->
%             qlc:e(Q)
%     end).
%
% new_id(Key) ->
%     mnesia:dirty_update_counter({counter, Key}, 1).
% 
% read(Oid) ->
%     transaction(fun() ->
%         mnesia:read(Oid)
%     end).
%     %naming(Result, Names).
% 
% read_all(Table) ->
%     Q = qlc:q([X || X <- mnesia:table(Table)]),
%     find(Q).
% 
% write(Rec) ->
%     transaction(fun() ->
%         mnesia:write(Rec)
%     end).
% 
% 
%     

% 
% 
