%% Feel free to use, reuse and abuse the code in this file.

-module(empweb_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

-include("empweb.hrl").


start() ->
    evman:start(),

    ensure_application_start(crypto),
    ensure_application_start(public_key),
    ensure_application_start(ssl),
    ensure_application_start(ranch),
    ensure_application_start(cowboy),
    ensure_application_start(mnesia),
    % ensure_application_start(amnesia),
    ensure_application_start(empdb),
    ensure_application_start(empweb).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile(empweb_dispatcher:dispatch()),
    start_listener(http, start_http, fun(Config)->[
        % nbacceptors
        proplists:get_value(nbacceptors,Config,?EMPWEB_NBACCEPTORS_DEF),
        % transopts
        [{port,proplists:get_value(port,Config,?EMPWEB_CPORT_HTTP_DEF)}],
        % protoopts
        [{env, [{dispatch, Dispatch}]}]
    ] end),

    start_listener(https, start_https, fun(Config)->[
        % nbacceptors
        proplists:get_value(nbacceptors,Config,?EMPWEB_NBACCEPTORS_DEF),
        % transopts
        [
            {port,      proplists:get_value(port,Config)},
            {certfile,  proplists:get_value(certfile,Config)},
            {keyfile,   proplists:get_value(keyfile,Config)},
            {password,  proplists:get_value(password,Config)}
        ],
        % protoopts
        [{env, [{dispatch, Dispatch}]}]
    ] end),

    %{ok, _} = cowboy:start_http(http, 100, [{port, 8001}], [
        %{env, [{dispatch, Dispatch}]}
    %]),



    ensure_start_link(empweb_sup).

stop(_State) ->
    ok.
start_listener(Name, Method, Function) ->
    case application:get_env(Name) of
        undefined ->
            fail_start_listener(Name);
        {ok, Config} when erlang:is_list(Config) ->
            erlang:apply(cowboy,Method,[Name|Function(Config)])
    end.


start_listener_(Name, Method, Function, Config) ->
    {ok, Empweb_sup} = ensure_start_link(empweb_sup),
    %%  Ниже мы стартуем стандартный cowboy:start_http[s]/4 как ребенка
    %%  наблюдателя. Это делается для того, чтобы перехватить
    %%  сообщения смерти в cowboy:start_listener.
    %%  Иначе сигналы будут перехвачены cowboy_sup
    %%  и просто выведется сообщение об ошибке
    io:format("~n~n ~p (~p)", [?MODULE, ?LINE]),
    case supervisor:start_child(ranch_listener_sup, {
        Name,{cowboy,Method,[Name|Function(Config)]},
        permanent,5000,worker,dynamic
    }) of
        {ok, _} ->
            io:format("~n~n ~p (~p) ~n~n", [?MODULE, ?LINE]),
            Empweb_sup;
        {error, _} ->
            io:format("~n~n ~p (~p) ~n~n", [?MODULE, ?LINE]),
            start_listener_(Name,Method,Function, incr_port(Config));
        X ->
            io:format("~n~n ~p (~p) ~n~n", [?MODULE, ?LINE]),
            error_logger:error_msg("~p: X ~p", [?MODULE, X])
    end.

%% @doc Проверяет запущено ли OTP-приложение,
%% Если не запущено то запускает его.
ensure_application_start(App) ->
    case application:start(App) of
        ok ->                               ok;
        {error, {already_started, App}} ->  ok
    end.

%% @doc Проверяет запущено ли процесс (gen_server),
%% Если не запущено то запускает его.
ensure_start_link(Proc) ->
    case Proc:start_link() of
        {ok, Pid} ->                    {ok, Pid};
        {error,{already_started,Pid}}-> {ok, Pid}
    end.

%% @doc Увеличивает значение порта.
incr_port(Options) ->
    [
        {port,proplists:get_value(port, Options)+1}
        |proplists:delete(port, Options)
    ].

fail_start_listener(Some)->
    error_logger:error_msg("~p: no ~p", [?MODULE, Some]).


% config(Par)->
%     case application:get_env(Par) of
%         {ok,Res} -> Res;
%         _ ->        undefined
%     end.
%
% config(Par, Opt)->
%     case application:get_env(Par) of
%         {ok, List} ->    proplists:get_value(Opt, List);
%         _ ->             undefined
%     end.
%
%
%
