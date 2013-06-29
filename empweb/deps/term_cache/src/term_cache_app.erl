%% Feel free to use, reuse and abuse the code in this file.

-module(term_cache_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(term_cache).

start(_Type, _Args) ->
    term_cache_sup:start_link().

stop(_State) ->
    ok.
