%%
%% @file    empweb_jsonapi_pers.erl
%%          "Контроллер" для функций работы с пользователями.
%%

-module(empweb_jsonapi_pers).
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
    empweb_jsonapi:handle_params(
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
            {ok,empweb_jsonapi:resp(empweb_biz_pers:register(Data#norm.return)),Hap}
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
    empweb_jsonapi:handle_params(
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
                empweb_jsonapi:resp(
                    empweb_biz_pers:pass([
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_at_least_one{rules=[
                #norm_rule{
                    key = id,
                    types = [integer]
                },
                #norm_rule{
                    key = login,
                    types = [string]
                },
                #norm_rule{
                    key = nick,
                    types = [string]
                }
            ]},
            #norm_rule{
                key = pass,
                types = [string]
            }
        ]),
        fun(Data)->
            %% Если login выполнился успешно,
            %%  то устанавливаем клиенту cookie.
            case empweb_biz_pers:login(Data#norm.return) of
                {ok, Body} ->
                    {ok,
                        (empweb_jsonapi:resp({ok, Body}))#empweb_resp{
                            cookies = [empweb_http:make_auth(Body)]
                        },
                        Hap
                    };
                Some ->
                    {ok, empweb_jsonapi:resp(Some), Hap}
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                types = [integer]
            }
        ]),
        fun(Data)->        
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_pers:logout([Auth | Data#norm.return])
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
        action=get_friend, params=Params, is_auth=true, pers_id=Pers_id
    } = Hap) ->
    ?evman_args(Hap, <<" = get friends">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = pers_id,
                required = false,
                types = [integer]
            }
            |empweb_norm:norm('get')
        ]),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_pers:get_friend([
                        {pers_id,
                            case proplists:get_value(pers_id, Data#norm.return) of
                                undefined ->
                                    Pers_id;
                                Pers_id ->
                                    Pers_id
                            end
                        } |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{
        action=add_friend, params=Params, is_auth=true, pers_id=Pers_id
    } = Hap) ->
    ?evman_args(Hap, <<" = add friend">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_at_least_one{rules=[
                #norm_rule{
                    key     = friend_id,
                    types   = [integer]
                },
                #norm_rule{
                    key     = friend_nick,
                    types   = [string]
                }
            ]}
        ]),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_pers:add_friend([
                        {pers_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{
        action=delete_friend, params=Params, is_auth=true, pers_id=Pers_id
    } = Hap) ->
    ?evman_args(Hap, <<" = delete friend">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_at_least_one{rules=[
                #norm_rule{
                    key     = friend_id,
                    types   = [integer]
                },
                #norm_rule{
                    key     = friend_nick,
                    types   = [string]
                }
            ]}
        ]),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_pers:delete_friend([
                        {pers_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

%%
%% Функция отрабатывает только если пользователь идентифицирован
%%
handle(_req, #empweb_hap{
        action=get_pers, params=Params, is_auth=true
    } = Hap) ->
    ?evman_args(Hap, <<" = get pers">>),
    % io:format("Params = ~p~n~n", [Params]),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = nick,
                required    = false,
                types       = empweb_norm:filter([string])
            },
        %% ----------------------------------------------------
        %% Имя
            #norm_rule{
                key         = fname,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
        %% Фамилия
            #norm_rule{
                key         = sname,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
        %% Род занятий
            #norm_rule{
                key         = empl,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
        %% Xобби
            #norm_rule{
                key         = hobby,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
        %% Интерес
            #norm_rule{
                key         = interest,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
        %% Oписание
            #norm_rule{
                key         = descr,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
        %% Регион pregion_id
            #norm_rule{
                key         = pregion_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
        %% Дата рождения
            #norm_rule{
                key         = birthday,
                required    = false,
                types       = empweb_norm:filter([nullable, unixdatetime])
            },
        %% Флаг пола
            #norm_rule{
                key         = ismale,
                required    = false,
                types       = empweb_norm:filter([nullable, boolean])
            },
        %% Комната
            #norm_rule{
                key         = live_room_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
        %% Положение в комнате
            #norm_rule{
                key         = live_room_pos,
                 required   = false,
                types       = empweb_norm:filter([nullable, 'float']) 
            },
        %% Сообщество
            #norm_rule{
                key         = live_community_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
        %% Эмоции пользователя.
            #norm_rule{
                key         = emotion_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = emotion_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
        %% Язык пользователя.
            #norm_rule{
                key         = lang_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = lang_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            }
            |empweb_norm:norm('get')
        ]),
        fun(Data)->
            {ok,empweb_jsonapi:resp(empweb_biz_pers:get(Data#norm.return)),Hap}
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
%     {ok,empweb_jsonapi:resp(empweb_biz_pers:get([])),Hap};

%%
%% Функция отрабатывает только если пользователь идентиф ицирован
%%
handle(_req, #empweb_hap{
        action=update_pers, params=Params, is_auth=true
    } = Hap) ->
    ?evman_args(Hap, <<" = update pers">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                required = false,
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
                %% Интерес
                    #norm_rule{
                        key         = interest,
                        types       = [nullable, string]
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
                        types = [nullable, unixdatetime]
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
                %% Положение в комнате
                    #norm_rule{
                        key = live_room_pos,
                        types = [nullable, 'float']
                    },
                %% Сообщество
                    #norm_rule{
                        key = live_community_id,
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
            {ok,empweb_jsonapi:resp(empweb_biz_pers:update(Data#norm.return)),Hap}
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Авторитет пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% handle(_req, #empweb_hap{action='get_all_authorities'} = Hap) ->
%     ?evman_args(Hap, <<" = get all authorities">>),
%     {ok,empweb_jsonapi:resp(empweb_biz_pers:get_authority([])),Hap};


handle(_req, #empweb_hap{action='get_authority', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = get authority">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = level,
                required = false,
                types = [integer]
            }|opt_norm('get')
        ]),
        fun(Data)->
            {ok,empweb_jsonapi:resp(empweb_biz_pers:get_authority(Data#norm.return)),Hap}
        end,
        Hap
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Семейное положение пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% handle(_req, #empweb_hap{action='get_all_mstatuses'} = Hap) ->
%     ?evman_args(Hap, <<" = get all mstatuses">>),
%     {ok,empweb_jsonapi:resp(empweb_biz_pers:get_mstatus([])),Hap};

handle(_req, #empweb_hap{action='get_mstatus', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = get mstatus">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            {ok,empweb_jsonapi:resp(empweb_biz_pers:get_mstatus(Data#norm.return)),Hap}
        end,
        Hap
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Статус пользователя пользователя: в сети \ не в сети.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% handle(_req, #empweb_hap{action='get_all_pstatuses'} = Hap) ->
%     ?evman_args(Hap, <<" = get all pstatuses">>),
%     {ok,empweb_jsonapi:resp(empweb_biz_pers:get_pstatus([])),Hap};

handle(_req, #empweb_hap{action='get_pstatus', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = get pstatus">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            {ok,empweb_jsonapi:resp(empweb_biz_pers:get_pstatus(Data#norm.return)),Hap}
        end,
        Hap
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Эмоции пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% handle(_req, #empweb_hap{action='get_all_emotions'} = Hap) ->
%     ?evman_args(Hap, <<" = get all emotions">>),
%     {ok,empweb_jsonapi:resp(empweb_biz_pers:get_emotion([])),Hap};

handle(_req, #empweb_hap{action='get_emotion', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = get emotion">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            {ok,empweb_jsonapi:resp(empweb_biz_pers:get_emotion(Data#norm.return)),Hap}
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
    {ok,empweb_jsonapi:forbidden(), Hap}.


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
        |empweb_norm:norm('get')
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
