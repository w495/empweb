%%
%% @file    jsonapi_pers.erl
%%          "Контроллер" для функций работы с пользователями.
%%

-module(jsonapi_blog).
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
init(_, Req, #empweb_hap{
        action          =   Action,
        params          =   Params,
        is_auth         =   Is_auth,
        pers_id         =   User_id,
        pers_perm_names =   User_perm_names
    } = Hap)->
    %%%
    %%% Это нужно, чтобы понять, какая функция дальше выполнится
    %%%
    ?evman_notice({hap, [
        {action,            Action},
        {params,            Params},
        {is_auth,           Is_auth},
        {pers_id,           User_id},
        {pers_perm_names,   User_perm_names}
    ]}, <<" = Hap">>),

    {ok,
        Req,
        #empweb_hap{
            action          =   Action,
            params          =   Params,
            is_auth         =   Is_auth,
            pers_id         =   User_id,
            pers_perm_names =   User_perm_names
        }
    }.


% {"params":{"sname":"sname1","nick":"nickname","city":"Иркутск","phone":"+380633612672","email":"em@il.com","pass":"password","birthday":1900,"description":"description","fname":"fname"},"fname":"register"}

handle(_req, #empweb_hap{action='create_blog', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = register">>),

    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = title,
                types = [string]
            },
            #norm_rule{
                key = content,
                types = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),

            ?debug("biz_pers:register(~p).", [Data#norm.return]),
            {ok,jsonapi:resp(biz_pers:register(Data#norm.return)),Hap}
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
            case biz_pers:login(Data#norm.return) of
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
                    biz_pers:logout([
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
                key = pers_id,
                types = [integer]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_pers:get_friends(Data#norm.return)),Hap}
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
                key = pers_id,
                types = [integer]
            },
            #norm_rule{
                key = friend_id,
                types = [integer]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_pers:add_friend(Data#norm.return)),Hap}
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
                key = pers_id,
                types = [integer]
            },
            #norm_rule{
                key = friend_id,
                types = [integer]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_pers:delete_friend(Data#norm.return)),Hap}
        end
    );

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{action=get_pers, params=Params, is_auth=true} = Hap) ->
    ?evman_args(Hap, <<" = get_pers">>),

    %%
    %% Для вызова данной функции достаточно иметь
    %% хотя бы один параметр из перечисленных
    %%
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_one{
                rules=[
                    #norm_rule{
                        key = id,
                        types = [integer]
                    },
                    #norm_rule{
                        key = nick,
                        types = [string]
                    },
                    #norm_rule{
                        key = name,
                        types = [string]
                    }
                ]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_pers:get(Data#norm.return)),Hap}
        end,
        Hap
    );


%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{action=get_all_perss, params=Params, is_auth=true} = Hap) ->
    ?evman_args(Hap, <<" = get_all_perss">>),

    {ok,jsonapi:resp(biz_pers:get(all)),Hap};

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{action=update_pers, params=Params, is_auth=true} = Hap) ->
    ?evman_args(Hap, <<" = update_pers">>),

    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                types = [integer]
            },
            #norm_at_least_one{
                rules=[
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
                        key = hobby,
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
                ]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_pers:update(Data#norm.return)),Hap}
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

