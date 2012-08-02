%%
%% @file    jsonapi_user.erl 
%%          "Контроллер" для функций работы с пользователями.
%%

-module(jsonapi_user).
-behavior(empweb_http_hap).

%% ---------------------------------------------------------------------------
%% Заголовочные файлы
%% ---------------------------------------------------------------------------

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


%% ---------------------------------------------------------------------------
%% Экспортируемые функции
%% ---------------------------------------------------------------------------

-export([
    init/3,
    handle/2,
    at_list_one/1,
    terminate/2
]).

%% ---------------------------------------------------------------------------
%% Внешние функции
%% ---------------------------------------------------------------------------

%%
%% @doc Инициализация запроса
%%
init(_, Req, #empweb_hap{action=Action, params=Params, is_auth=Is_auth} = Hap)->
    ?debug("~n init : Hap = ~p", [Hap]),
    {ok,
        Req,
        #empweb_hap{
            action=Action,
            params=Params,
            is_auth=Is_auth
        }
    }.

handle(_req, #empweb_hap{action='register', params=Params} = Hap) ->
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = nick,
                types = [string]
            },
            #norm_rule{
                key = description,
                types = [string]
            },
            #norm_rule{
                key = pass,
                types = [string]
            },
            #norm_rule{
                key = email,
                types = [email]
            },
            #norm_rule{
                key = phone,
                types = [string]
            },
            #norm_rule{
                key = fname,
                types = [string]
            },
            #norm_rule{
                key = sname,
                types = [string]
            },
            #norm_rule{
                key = birthday,
                types = [string]
            },
            #norm_rule{
                key = city,
                types = [string]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_user:register(Data#norm.return)),Hap}
        end
    );


handle(Req, #empweb_hap{action=login,  params=Params} = Hap) ->
    ?debug("Hap = ~p ~n", [Hap]),
    
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = nick,
                types = [string]
            },
            #norm_rule{
                key = pass,
                types = [string]
            }
        ]),
        fun(Data)->
            %% Если login выполнился успешно,
            %%  то устанавливаем клиенту cookie.
            case biz_user:login(Data#norm.return) of
                {ok, Body} ->
                    {ok,
                        (jsonapi:resp({ok, Body}))#empweb_resp{
                            cookies = [empweb_http:make_auth_cookie(Body)]
                        },
                        Hap
                    };
                Some ->
                    {ok, jsonapi:resp(Some), Hap}
            end
        end
    );

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(Req, #empweb_hap{action=logout,  params=Params, is_auth=true} = Hap) ->
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = nick,
                types = [string]
            }
        ]),
        fun(Data)->
            {ok,
                jsonapi:resp(
                    biz_user:logout([
                        {session_id, empweb_http:auth_cookie(Req)}
                        | Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{action=get_friends, params=Params, is_auth=true} = Hap) ->
    ?debug("Params = ~p", [Params]),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = user_id,
                types = [integer]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_user:get_friends(Data#norm.return)),Hap}
        end
    );

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{action=add_friend, params=Params, is_auth=true} = Hap) ->
    ?debug("Params = ~p", [Params]),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = user_id,
                types = [integer]
            },
            #norm_rule{
                key = friend_id,
                types = [integer]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_user:add_friend(Data#norm.return)),Hap}
        end
    );

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{action=delete_friend, params=Params, is_auth=true} = Hap) ->
    ?debug("Params = ~p", [Params]),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = user_id,
                types = [integer]
            },
            #norm_rule{
                key = friend_id,
                types = [integer]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_user:delete_friend(Data#norm.return)),Hap}
        end
    );

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{action=get_user, params=Params, is_auth=true} = Hap) ->
    %%
    %% Для вызова данной функции достаточно иметь
    %% хотя бы один параметр из перечисленных
    %%
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                required = false,
                types = [integer]
            },
            #norm_rule{
                key = nick,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = name,
                required = false,
                types = [string]
            }
        ]),
        fun(Data)->
            case at_list_one(Data#norm.return) of
                {ok, Tuple} ->
                    {ok,jsonapi:resp(biz_user:get(Tuple)),Hap};
                _ ->
                    {ok,jsonapi:resp({error, no_param}), Hap}
            end
        end
    );

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{action=update_user, params=Params, is_auth=true} = Hap) ->
    ?debug("~n update : Hap = ~p", [Hap]),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                types = [integer]
            },
            #norm_rule{
                key = nick,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = description,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = pass,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = email,
                required = false,
                types = [email]
            },
            #norm_rule{
                key = phone,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = hobby,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = fname,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = sname,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = birthday,
                required = false,
                types = [string]
            },
            #norm_rule{
                key = city,
                required = false,
                types = [string]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_user:update(Data#norm.return)),Hap}
        end
    );


handle(_req, Hap) ->
    {ok,jsonapi:forbidden(), Hap}.


terminate(_req, _hap)->
    ?debug("~n :: terminate ~p~n", [_hap]),
    
    ok.

%% ---------------------------------------------------------------------------
%% Внутрениие функции
%% ---------------------------------------------------------------------------

at_list_one([]) ->
    {error, no_param};
at_list_one([Return|_]) ->
    {ok, Return}.

