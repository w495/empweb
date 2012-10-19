%% Feel free to use, reuse and abuse the code in this file.

-module(handler_restapi).
-behaviour(cowboy_http_handler).
-export([
    init/3,
    rest_init/2,
    content_types_provided/2,
    to_json/2,
    known_methods/2,
    allowed_methods/2,
    is_authorized/2
]).


-include("empweb.hrl").

-include_lib("norm/include/norm.hrl").

%%
%% Описание записей событий и макросов
%%
-include_lib("evman/include/events.hrl").


%%
%% Трансформация для получения имени функции. 
%%
-include_lib("evman/include/evman_transform.hrl").

%% 
%% @doc Говорит о том, что будем использовать протокол Rest
%%
init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

%%
%% @doc Возвращает поддерживаемый список форматов и функции преобразования
%%
content_types_provided(Req, Hap) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, Hap}.

%%
%% @doc Инициализирует выполнение контроллеров, решает,
%% на какое действие, что делать.
%%
rest_init(Req, Opt)->
    {Rawp,      Req2}   = cowboy_http_req:path_info(Req),
    {Format,    Pinfo}  = format_info(Rawp),

    Hap = case Pinfo of
        %%  ... /pers/
        %%  ... /pers.json
        [<<"pers">>] ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   get_all_perss,
                    params          =   []
                };
        %%  ... /pers/1/
        %%  ... /pers/1.json
        [<<"pers">>, Id] ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   get_pers,
                    params          =   [{id, Id}]
                };


        _ ->
            #empweb_hap{
                handler         =   jsonapi_pers,
                action          =   get_all_perss,
                params          =   []
            }
    end,
    {ok, Req, Hap}.


%%
%% @doc Проверяет авторизован пользователь или нет.
%% Используется Basic аутентификация.
%%
is_authorized(Req, Hap) ->
    case cowboy_http_req:header('Authorization', Req) of
        {<<"Basic ", Binary/binary>>, _} ->
            [Login, Pass] = binary:split(base64:decode(Binary), <<":">>),
            case empdb_bizpers:login([{login, Login}, {pass, Pass}]) of
                 {ok, [{Ppl}]} ->
                    io:format("Ppl = ~p~n~n", [Ppl]),
                    {true, Req, Hap#empweb_hap{
                        is_auth         = true,
                        pers_id         = proplists:get_value(id, Ppl),
                        pers_perm_names = proplists:get_value(perm_names,Ppl)
                    }};
                _ ->
                    {{false, <<"Basic realm=\"Secure Area\"">>}, Req, Hap}
            end;
        _ ->
            {{false, <<"Basic realm=\"Secure Area\"">>}, Req, Hap}
    end.

%%
%% @doc Возвращает список известных методов
%%
known_methods(Req, Hap)->
    {['GET', 'HEAD', 'POST', 'DELETE'], Req, Hap}.

%%
%% @doc Возвращает список разрешенных методов
%%
allowed_methods(Req, Hap)->
    {['GET', 'HEAD', 'POST', 'DELETE'], Req, Hap}.

%%
%% @doc Проверяет Авторизован пользователь или нет.
%% Используется Basic аутентификация.
%%
to_json(Req, Hap) ->
    Empweb_resp = case handle_hap(Req, Hap) of
        {ok, R} -> R;
        {error, unknown_function} ->
            jsonapi:not_extended(unknown_function);
        {error, Error} ->
            jsonapi:internal_server_error(
                {[{unknown_error, jsonapi:format(Error)}]}
            )
    end,
    ?evman_debug({empweb_resp, Empweb_resp},<<"empweb response">>),
    Http_resp = empweb_http:resp(Empweb_resp),
    {ejson:encode(Http_resp#http_resp.body), Req, Hap}.

handle_hap(Req, #empweb_hap{}=Hap)->
    empweb_http:call(Req, Hap).

%%
%% @doc Определяет запрашиваемый формат. Возвращает Формат,
%% и путь к ресурсу без формата.
%%
format_info(Rawp)->
    [Hpwar|Tpwar] = lists:reverse(Rawp),
    case binary:split(Hpwar, <<".">>) of
        [Act, Fmt] ->
            {format(Fmt), lists:reverse([Act|Tpwar])};
        _ ->
            {json, Rawp}
    end.

%%
%% @doc Преобразует формат в вид атома.
%%
format(<<"txt">>)   -> text;
format(<<"text">>)  -> text;
format(<<"json">>)  -> json;
format(<<"xml">>)   -> xml;
format(Rawp)        -> Rawp.
