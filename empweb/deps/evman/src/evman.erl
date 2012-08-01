-module(evman).
-behaviour(evman_notifier).

-include("evman.hrl").
-define(EVENTNAME, ?MODULE).


%%% 
%%% evman_notifier API
%%% 
-export([
    start_link/0,
    start_link/1,
    add_handler/2,
    add_sup_handler/2,
    rem_handler/2,
    rem_sup_handler/2,
    get_handlers/0,
    info/1,
    note/1
]).


%%% -----------------------------------------------------------------------
%%% evman_notifier API
%%% -----------------------------------------------------------------------

start_link() ->
    evman_notifier:start_link(?EVENTNAME).

start_link(Args) ->
    evman_notifier:start_link(?EVENTNAME, Args).

add_handler(ModuleName, Args) ->
    evman_notifier:add_handler(?EVENTNAME, ModuleName, []).

add_sup_handler(ModuleName, Args) ->
    evman_notifier:add_sup_handler(?EVENTNAME, ModuleName, Args).

rem_handler(ModuleName, Args) ->
    evman_notifier:rem_handler(?EVENTNAME, ModuleName, Args).

rem_sup_handler(ModuleName, Args) ->
    evman_notifier:rem_sup_handler(?EVENTNAME, ModuleName, Args).

get_handlers() ->
    evman_notifier:get_handlers(?EVENTNAME).

info(Msg) ->
    ?debug("self() = ~p~n", [self()]),
    evman_notifier:info(?EVENTNAME, Msg).

note(Msg) ->
    evman_notifier:note(?EVENTNAME, Msg).
