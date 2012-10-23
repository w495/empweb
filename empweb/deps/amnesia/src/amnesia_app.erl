%% Feel free to use, reuse and abuse the code in this file.

-module(amnesia_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(amnesia).

start(_Type, _Args) ->
    amnesia:init(),
    amnesia_sup:start_link().

stop(_State) ->
    ok.
