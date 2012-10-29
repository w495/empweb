%% Feel free to use, reuse and abuse the code in this file.

-module(lgps_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(lgps).

start(_Type, _Args) ->
    lgps_sup:start_link().

stop(_State) ->
    ok.
