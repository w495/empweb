%%
%% @file    jsonapi_pers.erl
%%          "Контроллер" для функций работы с пользователями.
%%

-module(jsonapi_pers).
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
        auth            =   Auth,
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
            auth            =   Auth,
            action          =   Action,
            params          =   Params,
            is_auth         =   Is_auth,
            pers_id         =   User_id,
            pers_perm_names =   User_perm_names
        }
    }.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Сам пользователь непосредственно
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
                key = pass,
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
                types = [integer]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,jsonapi:resp(biz_pers:register(Data#norm.return)),Hap}
        end
    );


handle(Req, #empweb_hap{
        action  =   pass,
        params  =   Params,
        is_auth =   Is_auth,
        pers_id =   Pers_id,
        auth    =   Auth,
        pers_perm_names=Pperm_names
    }=Hap) ->
    ?evman_args(Hap, <<" = pass">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                types = [integer]
            },
            #norm_rule{
                key = email,
                types = [string]
            }
        ]),
        fun(Data)->
            {ok,
                jsonapi:resp(
                    biz_pers:pass([
                        Auth,
                        {pers_id, Pers_id},
                        {is_auth, Is_auth}
                        | Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );


handle(Req, #empweb_hap{action=login,  params=Params} = Hap) ->
    ?evman_args(Hap, <<" = login">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                types = [integer]
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
                            cookies = [empweb_http:make_auth(Body)]
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
handle(Req, #empweb_hap{
        action=logout,  params=Params, is_auth=true, auth=Auth
    } = Hap) ->
    ?evman_args(Hap, <<" = logout">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                types = [integer]
            }
        ]),
        fun(Data)->        
            {ok,
                jsonapi:resp(
                    biz_pers:logout([Auth | Data#norm.return])
                ),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Друзья пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{
        action=get_friends, params=Params, is_auth=true
    } = Hap) ->
    ?evman_args(Hap, <<" = get friends">>),

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
handle(_req, #empweb_hap{
        action=add_friend, params=Params, is_auth=true
    } = Hap) ->
    ?evman_args(Hap, <<" = add friend">>),
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
handle(_req, #empweb_hap{
        action=delete_friend, params=Params, is_auth=true
    } = Hap) ->
    ?evman_args(Hap, <<" = delete friend">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key     = pers_id,
                types   = [integer]
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
handle(_req, #empweb_hap{
        action=get_pers, params=Params, is_auth=true
    } = Hap) ->
    ?evman_args(Hap, <<" = get pers">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = room_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = community_id,
                required    = false,
                types       = [integer]
            }|jsonapi:norm('get')
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_pers:get(Data#norm.return)),Hap}
        end,
        Hap
    );

% 
% %%
% %% Функция отрабатывает только если пользователь идентифицирован
% %%
% handle(_req, #empweb_hap{
%         action=get_all_perss, params=Params, is_auth=true
%     } = Hap) ->
%     ?evman_args(Hap, <<" = get all perss">>),
%     {ok,jsonapi:resp(biz_pers:get([])),Hap};

%%
%% Функция отрабатывает только если пользователь идентиф ицирован
%%
handle(_req, #empweb_hap{
        action=update_pers, params=Params, is_auth=true
    } = Hap) ->
    ?evman_args(Hap, <<" = update pers">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                types = [integer]
            },
            #norm_at_least_one{
                rules=[
                %% ----------------------------------------------------
                %% Пароль
                    #norm_rule{
                        key = pass,
                        types = [string]
                    },
                %% Почта
                    #norm_rule{
                        key = email,
                        types = [nullable, email]
                    },
                %% Телефон
                    #norm_rule{
                        key = phone,
                        types = [nullable, integer]
                    },
                %% ----------------------------------------------------
                %% Имя
                    #norm_rule{
                        key = fname,
                        types = [nullable, string]
                    },
                %% Фамилия
                    #norm_rule{
                        key = sname,
                        types = [nullable, string]
                    },
                %% Род занятий
                    #norm_rule{
                        key = empl,
                        types = [nullable, string]
                    },
                %% Xобби
                    #norm_rule{
                        key = hobby,
                        types = [nullable, string]
                    },
                %% Oписание
                    #norm_rule{
                        key = descr,
                        types = [nullable, string]
                    },
                %% Регион pregion_id
                    #norm_rule{
                        key = pregion_id,
                        types = [nullable, integer]
                    },
                %% Дата рождения
                    #norm_rule{
                        key = birthday,
                        types = [nullable, datetime_unixtime]
                    },
                %% Флаг пола
                    #norm_rule{
                        key = ismale,
                        types = [nullable, boolean]
                    },
                %% Комната
                    #norm_rule{
                        key = live_room_id,
                        types = [nullable, integer]
                    },
                %% Сообщество
                    #norm_rule{
                        key = community_id,
                        types = [nullable, integer]
                    },
                %% Эмоции пользователя.
                    #norm_rule{
                        key = emotion_id,
                        types = [nullable, integer]
                    },
                    #norm_rule{
                        key = emotion_alias,
                        types = [nullable, string]
                    },
                %% Язык пользователя.
                    #norm_rule{
                        key = lang_id,
                        types = [nullable, integer]
                    },
                    #norm_rule{
                        key = lang_alias,
                        types = [nullable, string]
                    },
                %% Принимать, не принимать аукционы
                    #norm_rule{
                        key = allowauctionoffer,
                        types = [nullable, boolean]
                    }
                ]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_pers:update(Data#norm.return)),Hap}
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Авторитет пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% handle(_req, #empweb_hap{action='get_all_authorities'} = Hap) ->
%     ?evman_args(Hap, <<" = get all authorities">>),
%     {ok,jsonapi:resp(biz_pers:get_authority([])),Hap};


handle(_req, #empweb_hap{action='get_authority', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = get authority">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = level,
                required = false,
                types = [integer]
            }|opt_norm('get')
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_pers:get_authority(Data#norm.return)),Hap}
        end,
        Hap
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Семейное положение пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% handle(_req, #empweb_hap{action='get_all_mstatuses'} = Hap) ->
%     ?evman_args(Hap, <<" = get all mstatuses">>),
%     {ok,jsonapi:resp(biz_pers:get_mstatus([])),Hap};

handle(_req, #empweb_hap{action='get_mstatus', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = get mstatus">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            {ok,jsonapi:resp(biz_pers:get_mstatus(Data#norm.return)),Hap}
        end,
        Hap
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Статус пользователя пользователя: в сети \ не в сети.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% handle(_req, #empweb_hap{action='get_all_pstatuses'} = Hap) ->
%     ?evman_args(Hap, <<" = get all pstatuses">>),
%     {ok,jsonapi:resp(biz_pers:get_pstatus([])),Hap};

handle(_req, #empweb_hap{action='get_pstatus', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = get pstatus">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            {ok,jsonapi:resp(biz_pers:get_pstatus(Data#norm.return)),Hap}
        end,
        Hap
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Эмоции пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% handle(_req, #empweb_hap{action='get_all_emotions'} = Hap) ->
%     ?evman_args(Hap, <<" = get all emotions">>),
%     {ok,jsonapi:resp(biz_pers:get_emotion([])),Hap};

handle(_req, #empweb_hap{action='get_emotion', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = get emotion">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            {ok,jsonapi:resp(biz_pers:get_emotion(Data#norm.return)),Hap}
        end,
        Hap
    );

handle(_req, #empweb_hap{
        action=Action, params=Params, is_auth=Is_auth
    } = Hap) ->
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



opt_norm('get') ->
    [
        #norm_rule{
            key = alias,
            required = false,
            types = [string]
        },
        #norm_rule{
            key = id,
            required = false,
            types = [integer]
        }
        |jsonapi:norm('get')
    ];

opt_norm('create') ->
    [
        #norm_rule{
            key         = alias,
            required    = false,
            types       = [string]
        },
        #norm_rule{
            key         = name_ti,
            required    = false,
            types       = [integer]
        }
    ];

opt_norm('update') ->
    [
        #norm_rule{
            key         = id,
            required    = false,
            types       = [integer]
        }
        |opt_norm('create')
    ];

opt_norm(_) ->
    [].
