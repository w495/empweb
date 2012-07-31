-module(biz_session).


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

-include("biz_session.hrl").
-include_lib("stdlib/include/qlc.hrl").

start() ->
    %%% Зачем нам нужна амнезия ?
    biz_session_amnesia:start().

gen_uid(Login) ->
    Uid = <<(uuid:to_string(uuid:v4()))/binary,(convert:to_binary(Login))/binary>>,
    case ?MODULE:get(Uid) of
        [] -> Uid;
        _ -> gen_uid(Login)
    end.

new(#biz_session{uid=undefined} = Biz_session) ->
    ?MODULE:new(Biz_session#biz_session{
        uid = gen_uid(Biz_session#biz_session.nick)
    });

new(#biz_session{uid=Uid} = Biz_session) ->
    write(
        ?SESSION_TABLE_NAME,
        Biz_session#biz_session{
            uid = Uid,
            time=erlang:localtime()
        },
        write
    ),
    Uid.

get({uid, Uid}) ->
    ?MODULE:read({?SESSION_TABLE_NAME, Uid});

get(Uid) ->
    ?MODULE:read({?SESSION_TABLE_NAME, Uid}).

remove({nick, Uid}) ->
    ?MODULE:delete({?SESSION_TABLE_NAME, Uid});

remove({uid, Uid}) ->
    ?MODULE:delete({?SESSION_TABLE_NAME, Uid});

remove(Uid) ->
    ?MODULE:delete({?SESSION_TABLE_NAME, Uid}).

seconds(Item) ->
    calendar:datetime_to_gregorian_seconds(Item#biz_session.time).

expired(Curtime, Item) ->
    Curtime - seconds(Item) > ?BIZ_SESSION_EXPIRE_TIMEOUT.

remove_expired() ->
    Curtime = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    Function =
        fun() ->
            Query =
                qlc:q([
                    Item
                    || Item <-
                        mnesia:table(?SESSION_TABLE_NAME),
                        expired(Curtime, Item)
                ]),
            Set = qlc:e(Query),
            lists:foreach(
                fun(#biz_session{uid = Uid}) ->
                    mnesia:delete({?SESSION_TABLE_NAME, Uid})
                end,
                Set
            )
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
