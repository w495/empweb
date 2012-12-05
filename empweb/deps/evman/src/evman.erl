-module(evman).
-behaviour(evman_notifier).

-include("evman.hrl").
-define(EVENTNAME, ?MODULE).


%%% 
%%% evman_notifier API
%%% 
-export([
    start/0,
    start_link/0,
    start_link/1,
    add_handler/2,
    add_sup_handler/2,
    delete_handler/2,
    delete_sup_handler/2,
    which_handlers/0,
    format/1,
    format/2,
    llog/2,
    llog/3,
    info/1,
    note/1
]).

%%% -----------------------------------------------------------------------
%%% evman_notifier API
%%% -----------------------------------------------------------------------

start() ->
    evman_app:start().

start_link() ->
    evman_notifier:start_link(?EVENTNAME).

start_link(Args) ->
    evman_notifier:start_link(?EVENTNAME, Args).

add_handler(ModuleName, Args) ->
    evman_notifier:add_handler(?EVENTNAME, ModuleName, Args).

add_sup_handler(ModuleName, Args) ->
    evman_notifier:add_sup_handler(?EVENTNAME, ModuleName, Args).

delete_handler(ModuleName, Args) ->
    evman_notifier:delete_handler(?EVENTNAME, ModuleName, Args).

delete_sup_handler(ModuleName, Args) ->
    evman_notifier:delete_sup_handler(?EVENTNAME, ModuleName, Args).

which_handlers() ->
    evman_notifier:which_handlers(?EVENTNAME).

    
info(Msg) ->
    evman_notifier:info(?EVENTNAME, Msg).

note(Msg) ->
    evman_notifier:note(?EVENTNAME, Msg).




format(Trem) ->
    format("~p", [Trem]).

format(Format, List) when erlang:is_list(List) ->
    erlang:list_to_binary(io_lib:format(Format,List));

format(Format, Trem) ->
    format(Format, [Trem]).


llog(Level, Out) ->
    llog(Level, "~p", [Out]).

llog(Level, Format, List) when erlang:is_list(List) ->
    spawn_link(fun()->
        lager:log(
            Level,
            self(),
            Format ++ "~n",
            List
        )
    end);

llog(Level, Format, List)  ->
    llog(Level, Format, [List]).
