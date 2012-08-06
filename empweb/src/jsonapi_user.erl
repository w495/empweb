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
    %%%
    %%% Это нужно, чтобы понять, какая функция дальше выполнится
    %%%
    ?evman_notice({hap, [
        {action,    Action},
        {params,    Params},
        {is_auth,   Is_auth}
    ]}, <<" = Hap">>),

    {ok,
        Req,
        #empweb_hap{
            action=Action,
            params=Params,
            is_auth=Is_auth
        }
    }.


% {"params":{"sname":"sname1","nick":"nickname","city":"Иркутск","phone":"+380633612672","email":"em@il.com","pass":"password","birthday":1900,"description":"description","fname":"fname"},"fname":"register"}

handle(_req, #empweb_hap{action='register', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = register">>),

    ?evman_debug(Params, <<" = Params">>),
            
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
            ?evman_debug(Data, <<" = Data">>),

            ?debug("biz_user:register(~p).", [Data#norm.return]),
            {ok,jsonapi:resp(biz_user:register(Data#norm.return)),Hap}
        end
    );


handle(Req, #empweb_hap{action=login,  params=Params} = Hap) ->
    ?evman_args(Hap, <<" = login">>),
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
    ?evman_args(Hap, <<" = logout">>),
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
    ?evman_args(Hap, <<" = get_friends">>),

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
    ?evman_args(Hap, <<" = add_friend">>),

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
    ?evman_args(Hap, <<" = delete_friend">>),
    
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
    ?evman_args(Hap, <<" = get_user">>),

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
    ?evman_args(Hap, <<" = update_user">>),
    
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


handle(_req, #empweb_hap{action=Action, params=Params, is_auth=Is_auth} = Hap) ->
    ?evman_notice({hap, [
        {forbidden,     true},
        {action,        Action},
        {params,        Params},
        {is_auth,       Is_auth}
    ]}, <<" = forbidden">>),

    {ok,jsonapi:forbidden(), Hap}.


terminate(_req, Hap)->
    ?evman_args(Hap, <<" = terminate">>),
    
    ok.

%% ---------------------------------------------------------------------------
%% Внутрениие функции
%% ---------------------------------------------------------------------------

at_list_one([]) ->
    {error, no_param};
at_list_one([Return|_]) ->
    {ok, Return}.

