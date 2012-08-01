%% Feel free to use, reuse and abuse the code in this file.

-module(evman_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(evman).

start(_Type, _Args) ->
    evman_sup:start_link().

stop(_State) ->
    ok.
