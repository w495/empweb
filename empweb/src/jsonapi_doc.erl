%%
%% @file    jsonapi_doc.erl
%%          "Контроллер" для функций работы с документами.
%%

-module(jsonapi_doc).
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
    doc_norm/1,
    opt_norm/1,
    filter_owner/1,
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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          УТИЛИТАРНЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Тип разрешения: не рассмотрен, запрещена, разрешена
%%          TODO: добавить обработчиком
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle(_req, #empweb_hap{
        %% Возможно, просматривать список нужно разрешить всем.
        is_auth =   true,
        action  =   'get_oktype',
        params  =   Params
    } = Hap) ->
    ?evman_args(Hap, <<" = get oktype">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            {ok,
                jsonapi:resp(
                    biz_doc:get_oktype(
                        Data#norm.return,
                        proplists:get_value(fields, Data#norm.return, [])
                    )
                ),
                Hap
            }
        end,
        Hap
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Тип документа: Блог, коммент к блогу, галерея,
%%      фото, коммент к фото, attach descr.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        %% Возможно, просматривать список нужно разрешить всем.
        is_auth =   true,
        action  =   'get_doctype',
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = get doc type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_doctype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create_doctype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = create doc type">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_doctype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   update_doctype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = update doc type">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:update_doctype(Data#norm.return)),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы контента: обычный, эротический
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        %% Возможно, просматривать список нужно разрешить всем.
        is_auth =   true,
        action  =   'get_contype',
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = get contype">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_contype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create_contype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = create contype">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_contype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   update_contype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = update contype">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:update_contype(Data#norm.return)),
                Hap
            }
        end
    );


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Тип доступа к контенту контента (блога и галереи):
%%  приватный, дружеский, открытый.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        %% Возможно, просматривать список нужно разрешить всем.
        is_auth =   true,
        action  =   get_acctype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = get acctype">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_acctype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create_acctype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = create acctype">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_acctype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   update_acctype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = update acctype">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:update_acctype(Data#norm.return)),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы чат-комнат. (страна, тюрьма, ад, рай)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        %% Возможно, просматривать список нужно разрешить всем.
        is_auth =   true,
        action  =   get_roomtype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = get room type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_roomtype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create_roomtype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = create room type">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_roomtype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   update_roomtype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = update room type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:update_roomtype(Data#norm.return)),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Список языков чата.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        %% Возможно, просматривать список нужно разрешить всем.
        is_auth =   true,
        action  =   get_chatlang,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = get chatlang">>),
    io:format("Params = ~p~n~n", [Params]),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            io:format("Data#norm.return = ~p~n~n", [Data#norm.return]),
            {ok,
                jsonapi:resp(biz_doc:get_chatlang(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create_chatlang,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = create chatlang">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_chatlang(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   update_chatlang,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = update chatlang">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:update_chatlang(Data#norm.return)),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Список режимов комнаты
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        %% Возможно, просматривать список нужно разрешить всем.
        is_auth =   true,
        action  =   get_regimen,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = get regimen">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_regimen(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create_regimen,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = create regimen">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_regimen(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   update_regimen,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = update regimen">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:update_regimen(Data#norm.return)),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Дерево тем комнаты
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        %% Возможно, просматривать список нужно разрешить всем.
        is_auth =   true,
        action  =   get_topic,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = get topic">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_topic(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create_topic,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = create topic">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_topic(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   update_topic,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = update topic">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:update_topic(Data#norm.return)),
                Hap
            }
        end
    );
    
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы сообществ (обычные, тайные)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        %% Возможно, просматривать список нужно разрешить всем.
        is_auth =   true,
        action  =   get_communitytype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = get community type">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_communitytype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create_communitytype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = create community type">>),
    jsonapi:handle_params(
        norm:norm(Params, opt_norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_communitytype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   update_communitytype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = update community type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:update_communitytype(Data#norm.return)),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы сообщений
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        %% Возможно, просматривать список нужно разрешить всем.
        is_auth =   true,
        action  =   'get_messagetype',
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = get message type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_messagetype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create_messagetype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = create message type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_messagetype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   update_messagetype,
        params  =   Params
    } = Hap) ->
    ?evman_args([Hap], <<" = update message type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, opt_norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:update_messagetype(Data#norm.return)),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          ЗНАЧИМЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Блоги
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Возвращает список блогов на основе параметров
%%
handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   get_blog,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get blog[s]">>),
    jsonapi:handle_params(
        norm:norm(Params, doc_norm('get')),
        fun(Data)->
            {ok,
                jsonapi:resp(
                    biz_doc:get_blog(
                        filter_owner([
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
        action  =   create_blog,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create blog">>),
    jsonapi:handle_params(
        norm:norm(Params, doc_norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:create_blog([
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
        action  =   update_blog,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update blog">>),
    jsonapi:handle_params(
        norm:norm(Params, doc_norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:update_blog([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Посты \ коменты
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   get_post,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get post[s]">>),
    jsonapi:handle_params(
        norm:norm(Params, doc_norm('get')),
        fun(Data)->
            {ok,
                jsonapi:resp(
                    biz_doc:get_post(
                        filter_owner([
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
        action  =   create_post,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create post">>),
    ?debug("Params = ~p~n", [Params]),
    jsonapi:handle_params(
        norm:norm(Params, doc_norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:create_post([
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
        action  =   update_post,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update post">>),
    jsonapi:handle_params(
        norm:norm(Params, doc_norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:update_post([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Чат-комнаты (комнаты)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   get_room,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get room">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
                #norm_rule{
                    key         = type_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = ulimit,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = topic_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = chatlang_id,
                    required    = false,
                    types       = [integer]
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
                |doc_norm('get')
            ]
        ),
        fun(Data)->
            {ok,
                jsonapi:resp(
                    biz_doc:get_room(
                        filter_owner([
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
        action  =   create_room,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create room">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
                #norm_rule{
                    key         = type_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = ulimit,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = topic_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = chatlang_id,
                    required    = false,
                    types       = [integer]
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
                |doc_norm('create')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:create_room([
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
        action  =   update_room,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update room">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
                #norm_rule{
                    key         = type_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = ulimit,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = topic_id,
                    required    = false,
                    types       = [integer]
                },
                #norm_rule{
                    key         = chatlang_id,
                    required    = false,
                    types       = [integer]
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
                |doc_norm('update')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:update_room([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Сообщества
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   get_community,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get community">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = treasury,
                required    = false,
                types       = [float]
            },
            #norm_rule{
                key         = slogan,
                required    = false,
                types       = [string]
            }
            |doc_norm('get')
        ]),
        fun(Data)->
            {ok,
                jsonapi:resp(
                    biz_doc:get_community(
                        filter_owner([
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
        action  =   create_community,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create community">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = treasury,
                required    = false,
                types       = [float]
            },
            #norm_rule{
                key         = slogan,
                required    = false,
                types       = [string]
            }
            |doc_norm('create')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:create_community([
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
        action  =   update_community,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update community">>),

    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = treasury,
                required    = false,
                types       = [float]
            },
            #norm_rule{
                key         = slogan,
                required    = false,
                types       = [string]
            }
            |doc_norm('update')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:update_community([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Сообщения
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   get_message,
        params  =   Params
    } = Hap) ->
    %% 
    %% Pers_id при получении сообщения смысла не имеет.
    %% т.к. мы смысл имеет получать как своих сообщений, 
    %% так и чужих, которые отпарвлены мне.
    %% Отправителя и получателя надо указывать явно.
    %%
    ?evman_args([Hap], <<" = get message">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [nulluble, integer]
            },
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            }
            | doc_norm('get')
        ]),
        fun(Data)->
            {ok,
                jsonapi:resp(
                    biz_doc:get_message(
                        Data#norm.return,
                        proplists:get_value(fields, Data#norm.return, [])
                    )
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create_message,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create message">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = reader_id,
                types       = [integer]
            },
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            }
            | doc_norm('create')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:create_message([
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
        action  =   update_message,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update message">>),

    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            }
            | doc_norm('update')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:update_message(Data#norm.return)
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   delete_message_for_me,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update message">>),

    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            }
            | doc_norm('update')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:delete_message_for_me(Data#norm.return)
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   delete_message_from_me,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update message">>),

    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            }
            | doc_norm('update')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:delete_message_from_me(Data#norm.return)
                ),
                Hap
            }
        end
    );

% handle(_req, #empweb_hap{
%         is_auth =   true,
%         action  =   delete_message,
%         params  =   Params
%     } = Hap) ->
%     %%
%     %% Pers_id при получении сообщения смысла не имеет.
%     %% т.к. мы смысл имеет получать как своих сообщений,
%     %% так и чужих, которые отпарвлены мне.
%     %% Отправителя и получателя надо указывать явно.
%     %%
%     ?evman_args([Hap], <<" = delete message">>),
%     jsonapi:handle_params(
%         %% проверка входных параметров и приведение к нужному типу
%         norm:norm(Params, [
%             #norm_rule{
%                 key         = reader_id,
%                 required    = false,
%                 types       = [integer]
%             },
%             #norm_rule{
%                 key         = type_id,
%                 required    = false,
%                 types       = [nulluble, integer]
%             }
%             | doc_norm('update')
%         ]),
%         fun(Data)->
%             ?evman_debug(Data, <<" = Data">>),
%             {ok,
%                 jsonapi:resp(
%                     biz_doc:update_message([
%                         {owner_id, Pers_id}
%                         |Data#norm.return
%                     ])
%                 ),
%                 Hap
%             }
%         end
%     );
    
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

    {ok,jsonapi:forbidden(), Hap}.


terminate(_req, Hap)->
    ?evman_args([Hap], <<" = terminate">>),
    ok.

%% ===========================================================================
%% Внутрениие функции
%% ===========================================================================

%%
%% @spec filter_owner([{atom(), any()}]) -> [{atom(), any()}]
%%
%% @doc Фильтрует набор аргументов, на предмет того, кто является владельем
%%      документа. Если нет параметра owner_id, но есть параметр pers_id,
%%      то тогда, owner_id принимает значение pers_id.
%%      В данном случае owner_id указывается, если мы хотим узнать
%%      докуметы конкретного пользователя, pers_id --- список своих документов.
%%
filter_owner(Params) ->
    case {
        proplists:get_value(owner_id, Params),
        proplists:get_value(pers_id, Params)
    } of
        {all     , _      } ->
            proplists:delete(pers_id, proplists:delete(owner_id, Params));
        {undefined, Pers_id} ->
            [{owner_id, Pers_id}|proplists:delete(pers_id, Params)];
        {_owner_id, _      } ->
            proplists:delete(pers_id, Params)
    end.

%%
%% @doc Общие функции нормировки для всех документов
%%
doc_norm('get') ->
    [
        #norm_rule{
            key = id,
            required = false,
            types = [normcond([integer])]
        },
        #norm_rule{
            key = doctype_id,
            required = false,
            types = [nullable, integer]
        },
        #norm_rule{
            key = oktype_id,
            required = false,
            types = [nullable, integer]
        },
        #norm_rule{
            key = contype_id,
            required = false,
            types = [nullable, integer]
        },
        #norm_rule{
            key         = parent_id,
            required    = false,
            types       = [integer]
        },
        #norm_rule{
            key = owner_id,
            required = false,
            types = [nullable, allable, integer]
        }
        | jsonapi:norm('get')
    ];

doc_norm('create') ->
    [
        #norm_rule{
            key         = head,
            required    = false,
            types       = [string]
        },
        #norm_rule{
            key         = body,
            required    = false,
            types       = [string]
        },
        #norm_rule{
            key         = parent_id,
            required    = false,
            types       = [integer]
        },
        #norm_rule{
            key         = position,
            required    = false,
            types       = [integer]
        },
        #norm_rule{
            key         = doctype_id,
            required    = false,
            types       = [integer]
        },
        #norm_rule{
            key         = contype_id,
            required    = false,
            types       = [integer]
        },
        #norm_rule{
            key         = oktype_id,
            required    = false,
            types       = [integer]
        }
    ];

doc_norm('update') ->
    [
        #norm_rule{
            key         = id,
            required    = false,
            types       = [integer]
        }
        |doc_norm('create')
    ];

doc_norm(_) ->
    [].


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
            types = [any]
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
