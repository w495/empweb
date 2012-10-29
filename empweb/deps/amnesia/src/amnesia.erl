%% Created: 16.01.2009
%% Description: Source from rabbitmq
-module(amnesia).

%%
%% Exported Functions
%%
-export([
    start_link/1,
    start_link/0,
    status/0,
    ensure_mnesia_dir/0,
    ensure_mnesia_running/0,
    cluster/1,
    reset/0,
    force_reset/0
]).


-export([
    transaction/1,
    write/3,
    delete/1,
    delete/2,
    table/1,
    read/1
]).



start_link()->
    amnesia_sup:start_link().

start_link(Opt)->
    amnesia_sup:start_link(Opt).

status()->
    amnesia_worker:status().

ensure_mnesia_dir()->
    amnesia_worker:ensure_mnesia_dir().


ensure_mnesia_running()->
    amnesia_worker:ensure_mnesia_running().

cluster(Opt)->
    amnesia_worker:cluster(Opt).

reset()->
    amnesia_worker:reset().

force_reset()->
    amnesia_worker:force_reset().

%%%%


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

%%%%

table(Tname) ->
    mnesia:table(Tname).

delete(Tname, Uid) ->
    mnesia:delete({Tname, Uid}).

