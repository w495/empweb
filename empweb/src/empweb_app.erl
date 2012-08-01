%% Feel free to use, reuse and abuse the code in this file.

-module(empweb_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

-include("empweb.hrl").


start() ->
    ensure_application_start(crypto),
    ensure_application_start(public_key),
    ensure_application_start(ssl),
    ensure_application_start(mnesia),
    ensure_application_start(cowboy),
    ensure_application_start(evman),
    ensure_application_start(amnesia),
    ensure_application_start(empdb),
    ensure_application_start(empweb).

start(_Type, _Args) ->
    start_listener(http, fun(Config)->[
        % nbacceptors
        proplists:get_value(nbacceptors,Config,?EMPWEB_NBACCEPTORS_DEF),
        % transport
        cowboy_tcp_transport,
        % transopts
        [{port,proplists:get_value(port,Config,?EMPWEB_CPORT_HTTP_DEF)}],
        % protocol
        cowboy_http_protocol,
        % protoopts
        [{dispatch, dispatcher:dispatch()}]
    ] end),
    start_listener(https, fun(Config)->[
        % nbacceptors
        proplists:get_value(nbacceptors,Config,?EMPWEB_NBACCEPTORS_DEF),
        % transport
        cowboy_tcp_transport,
        % transopts
        [
            {port,      proplists:get_value(port,Config)},
            {certfile,  proplists:get_value(certfile,Config)},
            {keyfile,   proplists:get_value(keyfile,Config)},
            {password,  proplists:get_value(password,Config)}
        ],
        % protocol
        cowboy_http_protocol,
        % protoopts
        [{dispatch, dispatcher:dispatch()}]
    ] end),

    ensure_start_link(empweb_sup).

stop(_State) ->
    ok.
start_listener(Name, Function) ->
    case application:get_env(Name) of
        undefined ->
            fail_start_listener(Name);
        {ok, Config} when erlang:is_list(Config) ->
            start_listener(Name, Function, Config)
    end.

start_listener(Name, Function, Config) ->
    {ok, Empweb_sup} = ensure_start_link(empweb_sup),
    %%  Ниже мы стартуем стандартный cowboy:start_listener/6 как ребенка 
    %%  наблюдателя. Это делается для того, чтобы перехватить 
    %%  сообщения смерти в cowboy:start_listener. 
    %%  Иначе сигналы будут перехвачены cowboy_sup 
    %%  и просто выведется сообщение об ошибке 
    case supervisor:start_child(Empweb_sup, {
        Name,{cowboy,start_listener,[Name|Function(Config)]},
        permanent,5000,worker,dynamic
    }) of
        {ok, _} -> Empweb_sup;
        {error, _} ->
            start_listener(Name,incr_port(Config),Function);
        X ->
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
