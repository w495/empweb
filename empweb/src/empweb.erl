%% Feel free to use, reuse and abuse the code in this file.

-module(empweb).
-export([start/0, start/2, stop/1]).

start() ->
    empweb_app:start().

start(Type, Args) ->
    empweb_app:start(Type, Args).

stop(State) ->
    empweb_app:stop(State).

