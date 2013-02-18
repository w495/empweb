%%
%% @file    empweb_jsonapi_doc.erl
%%          "Контроллер" для функций работы с документами.
%%

-module(empweb_jsonapi_room).
-behavior(empweb_http_hap).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Определения общие для всего приложения
%%
-include("empweb.hrl").

%%
%% Описание структур нормировки полей
%%
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
%% Структры для работы с запросами к базе данных
%%
-include_lib("empdb/include/empdb.hrl").


%% ===========================================================================
%% Экспортируемые функции
%% ===========================================================================

-export([
    init/3,
    handle/2,
    terminate/2
]).

%% ===========================================================================
%% Внешние функции
%% ===========================================================================

%%
%% @doc Инициализация запроса
%%
init(_, Req, #empweb_hap{
        action          =   Action,
        params          =   Params,
        is_auth         =   Is_auth,
        pers_id         =   Pid,
        pers_perm_names =   Pns
    } = _hap)->
    %%
    %% Это нужно, чтобы понять, какая функция дальше выполнится
    %%
    ?evman_notice({hap, [
        {action,            Action},
        {params,            Params},
        {is_auth,           Is_auth},
        {pers_id,           Pid},
        {pers_perm_names,   Pns}
    ]}, <<" = Hap">>),

    {ok,
        Req,
        #empweb_hap{
            action          =   Action,
            params          =   Params,
            is_auth         =   Is_auth,
            pers_id         =   Pid,
            pers_perm_names =   Pns
        }
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Чат-комнаты (комнаты)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   get_blogs,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get blog[s]">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer]),
                nkey        = 'pers.citizen_room_id'
            },
            #norm_rule{
                key         = isweek,
                required    = false,
                types       = empweb_norm:filter([nullable, boolean])
            }
            |empweb_norm:norm('get')
        ]),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_room:get_blogs(Data#norm.return)
                ),
                Hap
            }
        end
    );
    
handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   get,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get room">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
        
            #norm_rule{
                key         = back_file_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = back_path,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = wall_file_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = wall_path,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = flag_file_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = flag_path,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = arms_file_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = arms_path,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },

            #norm_rule{
                key         = roomtype_id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = roomtype_alias,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = ulimit,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = topic_id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = chatlang_id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = chatlang_alias,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = treasury,
                required    = false,
                types       = empweb_norm:filter([float])
            },
            #norm_rule{
                key         = weather,
                required    = false,
                types       = empweb_norm:filter([string])
            },
            #norm_rule{
                key         = slogan,
                required    = false,
                types       = empweb_norm:filter([string])
            }
            |empweb_norm_doc:norm('get')
        ]),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_room:get(
                        empweb_norm:filter_owner([
                            {pers_id, Pers_id}
                            |Data#norm.return
                        ]),
                        proplists:get_value(fields, Data#norm.return, [])
                    )
                ),
                Hap
            }
        end
    );


handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   count,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get room">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [


            #norm_rule{
                key         = back_file_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = back_path,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = wall_file_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = wall_path,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = flag_file_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = flag_path,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = arms_file_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = arms_path,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },

            #norm_rule{
                key         = regimen_id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = regimen_alias,
                required    = false,
                types       = empweb_norm:filter([string])
            },
            #norm_rule{
                key         = roomtype_id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = roomtype_alias,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = ulimit,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = topic_id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = chatlang_id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = chatlang_alias,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = treasury,
                required    = false,
                types       = empweb_norm:filter([float])
            },
            #norm_rule{
                key         = weather,
                required    = false,
                types       = empweb_norm:filter([string])
            },
            #norm_rule{
                key         = slogan,
                required    = false,
                types       = empweb_norm:filter([string])
            }
            |empweb_norm_doc:norm('get')
        ]),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_room:count(
                        empweb_norm:filter_owner([
                            {pers_id, Pers_id}
                            |Data#norm.return
                        ])
                    )
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   join,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = join room">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                types       = [integer]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_room:join([
                        {pers_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );


handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create room">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [


                #norm_rule{
                    key         = back_file_id,
                    required    = false,
                    types       = [nullable, integer]
                },
                #norm_rule{
                    key         = back_path,
                    required    = false,
                    types       = [nullable, string]
                },

                #norm_rule{
                    key         = wall_file_id,
                    required    = false,
                    types       = [nullable, integer]
                },
                #norm_rule{
                    key         = wall_path,
                    required    = false,
                    types       = [nullable, string]
                },
                #norm_rule{
                    key         = flag_file_id,
                    required    = false,
                    types       = [nullable, integer]
                },
                #norm_rule{
                    key         = flag_path,
                    required    = false,
                    types       = [nullable, string]
                },
                #norm_rule{
                    key         = arms_file_id,
                    required    = false,
                    types       = [nullable, integer]
                },
                #norm_rule{
                    key         = arms_path,
                    required    = false,
                    types       = [nullable, string]
                },

                #norm_rule{
                    key         = regimen_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = regimen_alias,
                    required    = false,
                    types       = [string]
                },
                #norm_rule{
                    key         = roomtype_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = roomtype_alias,
                    required    = false,
                    types       = [string]
                },
                #norm_rule{
                    key         = ulimit,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = chatlang_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = chatlang_alias,
                    required    = false,
                    types       = [string]
                },
                #norm_rule{
                    key         = treasury,
                    required    = false,
                    types       = [float]
                },
                #norm_rule{
                    key         = weather,
                    required    = false,
                    types       = [string]
                },
                #norm_rule{
                    key         = slogan,
                    required    = false,
                    types       = [string]
                }
                |empweb_norm_doc:norm('create')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_room:create([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])  
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   update,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update room">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [


                #norm_rule{
                    key         = back_file_id,
                    required    = false,
                    types       = [nullable, integer]
                },
                #norm_rule{
                    key         = back_path,
                    required    = false,
                    types       = [nullable, string]
                },

                #norm_rule{
                    key         = wall_file_id,
                    required    = false,
                    types       = [nullable, integer]
                },
                #norm_rule{
                    key         = wall_path,
                    required    = false,
                    types       = [nullable, string]
                },
                #norm_rule{
                    key         = flag_file_id,
                    required    = false,
                    types       = [nullable, integer]
                },
                #norm_rule{
                    key         = flag_path,
                    required    = false,
                    types       = [nullable, string]
                },
                #norm_rule{
                    key         = arms_file_id,
                    required    = false,
                    types       = [nullable, integer]
                },
                #norm_rule{
                    key         = arms_path,
                    required    = false,
                    types       = [nullable, string]
                },

                
                #norm_rule{
                    key         = regimen_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = regimen_alias,
                    required    = false,
                    types       = [string]
                },
                #norm_rule{
                    key         = roomtype_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = roomtype_alias,
                    required    = false,
                    types       = [string]
                },
                #norm_rule{
                    key         = ulimit,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = chatlang_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = chatlang_alias,
                    required    = false,
                    types       = [string]
                },
                #norm_rule{
                    key         = treasury,
                    required    = false,
                    types       = [float]
                },
                #norm_rule{
                    key         = weather,
                    required    = false,
                    types       = [string]
                },
                #norm_rule{
                    key         = slogan,
                    required    = false,
                    types       = [string]
                }
                |empweb_norm_doc:norm('update')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_room:update([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );


handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   add_topic,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update room">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key         = room_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = topic_id,
                required    = false,
                types       = [integer]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_room:add_topic(Data#norm.return)
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   delete_topic,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update room">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key         = room_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = topic_id,
                required    = false,
                types       = [integer]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_room:delete_topic(Data#norm.return)
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   delete,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update room">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('delete')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_room:delete([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% То что не прошло сопоставления с образцом.
%% В частности, неавторизованого пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle(_req, #empweb_hap{
        is_auth =   Is_auth,
        action  =   Action,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_notice({hap, [
        {forbidden,     true},
        {action,        Action},
        {params,        Params},
        {pers_id,       Pers_id},
        {is_auth,       Is_auth}
    ]}, <<" = forbidden">>),

    {ok,empweb_jsonapi:forbidden(), Hap}.


terminate(_req, Hap)->
    ?evman_args([Hap], <<" = terminate">>),
    ok.

%% ===========================================================================
%% Внутрениие функции
%% ===========================================================================

