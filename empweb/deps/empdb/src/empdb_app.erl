%% @copyright 2013 Empire
%% @author Илья w-495 Никитин
%% @doc Модуль, стартующий приложение.
%%

-module(empdb_app).

-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1
]).

start() ->
    application:start(empdb).

start(_Type, _Args) ->
    empdb_sup:start_link().

stop(_State) ->
    ok.
