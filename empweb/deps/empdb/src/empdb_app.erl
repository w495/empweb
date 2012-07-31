%% Feel free to use, reuse and abuse the code in this file.

-module(empdb_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(psqlcp),
    application:start(empdb).

start(_Type, _Args) ->
    empdb_sup:start_link().

stop(_State) ->
    ok.
