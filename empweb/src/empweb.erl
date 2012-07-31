%% Feel free to use, reuse and abuse the code in this file.

-module(empweb).
-export([start/0, start/2, stop/1]).

start() ->
    empire_web_app:start().

start(Type, Args) ->
    empire_web_app:start(Type, Args).

stop(State) ->
    empire_web_app:stop(State).

