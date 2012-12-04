%%
%% @file    empweb_jsonapi_doc.erl
%%          "Контроллер" для функций работы с документами.
%%

-module(empweb_jsonapi_doc).
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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          УТИЛИТАРНЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:get_doctype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:create_doctype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:update_doctype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:get_contype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:create_contype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:update_contype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:get_acctype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:create_acctype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:update_acctype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:get_roomtype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:create_roomtype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:update_roomtype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            io:format("Data#norm.return = ~p~n~n", [Data#norm.return]),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:get_chatlang(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:create_chatlang(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:update_chatlang(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:get_regimen(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:create_regimen(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:update_regimen(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, [
            #norm_rule{
                key         = parent_id,
                required    = false,
                types       = [nullable, integer]
            }
            | empweb_norm_opt:norm('get')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:get_topic(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:create_topic(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:update_topic(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:get_communitytype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_opt:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:create_communitytype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:update_communitytype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:get_messagetype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:create_messagetype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_opt:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(empweb_biz_doc:update_messagetype(Data#norm.return)),
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('get')),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:get_blog(
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
        action  =   get_my_blog,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get blog[s]">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('get')),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:get_blog([
                            {owner_id, Pers_id}
                            |Data#norm.return
                        ],
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:create_blog([
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:update_blog([
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
        action  =   delete_blog,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update blog">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('delete')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:delete_blog([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Посты 
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   get_post,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get post[s]">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('get')),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:get_post(
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
        action  =   get_my_post,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get post[s]">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('get')),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:get_post([
                            {owner_id, Pers_id}
                            |Data#norm.return
                        ],
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:create_post([
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
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:update_post([
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
        action  =   delete_post,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update post">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('delete')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:delete_post([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Коменты
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   get_comment,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get comment[s]">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('get')),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:get_comment(
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
        action  =   get_my_comment,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get comment[s]">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('get')),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:get_comment([
                            {owner_id, Pers_id}
                            |Data#norm.return
                        ],
                        proplists:get_value(fields, Data#norm.return, [])
                    )
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   create_comment,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create comment">>),
    ?debug("Params = ~p~n", [Params]),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('create')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:create_comment([
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
        action  =   update_comment,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update comment">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('update')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:update_comment([
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
        action  =   delete_comment,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update comment">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('delete')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:delete_comment([
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
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
            ]
        ),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:get_room(
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
        action  =   count_room,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get room">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
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
            ]
        ),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:count_room(
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
        action  =   join_room,
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
                    empweb_biz_doc:join_room([
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
        action  =   create_room,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create room">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
                #norm_rule{
                    key         = roomtype_id,
                    required    = false,
                    types       = [integer]
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
                    empweb_biz_doc:create_room([
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
                #norm_rule{
                    key         = roomtype_id,
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
                |empweb_norm_doc:norm('update')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:update_room([
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
        action  =   add_room_topic,
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
                    empweb_biz_doc:add_room_topic(Data#norm.return)
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   delete_room_topic,
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
                    empweb_biz_doc:delete_room_topic(Data#norm.return)
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   delete_room,
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
                    empweb_biz_doc:delete_room([
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = communitytype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = communitytype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, atom])
            },
            #norm_rule{
                key         = treasury,
                required    = false,
                types       = empweb_norm:filter([float])
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
                    empweb_biz_doc:get_community(
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
        action  =   count_community,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = count community">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = communitytype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = communitytype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, atom])
            },
            #norm_rule{
                key         = treasury,
                required    = false,
                types       = empweb_norm:filter([float])
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
                    empweb_biz_doc:count_community(
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
        action  =   create_community,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create community">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = communitytype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = communitytype_alias,
                required    = false,
                types       = [nullable, atom]
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
            |empweb_norm_doc:norm('create')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:create_community([
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

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = communitytype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = communitytype_alias,
                required    = false,
                types       = [nullable, atom]
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
            |empweb_norm_doc:norm('update')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:update_community([
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
        action  =   delete_community,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update community">>),
    empweb_jsonapi:handle_params(
        norm:norm(Params, empweb_norm_doc:norm('delete')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:delete_community([
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
        action  =   count_message_types,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    %%
    %% Pers_id при получении сообщения смысла не имеет.
    %% т.к. мы смысл имеет получать как своих сообщений,
    %% так и чужих, которые отпарвлены мне.
    %% Отправителя и получателя надо указывать явно.
    %%
    ?evman_args([Hap], <<" = count message types">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_doc:norm('get')),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:count_message_types(
                        [{pers_id, Pers_id}|Data#norm.return]
                    )
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   get_message,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    %% 
    %% Pers_id при получении сообщения смысла не имеет.
    %% т.к. мы смысл имеет получать как своих сообщений, 
    %% так и чужих, которые отпарвлены мне.
    %% Отправителя и получателя надо указывать явно.
    %%
    ?evman_args([Hap], <<" = get message">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = reader_nick,
                required    = false,
                types       = [nulluble, string]
            },
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [nulluble, integer]
            }
            | empweb_norm_doc:norm('get')
        ]),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:get_message(
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
        action  =   get_message_for_me,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    %%
    %% Pers_id при получении сообщения смысла не имеет.
    %% т.к. мы смысл имеет получать как своих сообщений,
    %% так и чужих, которые отпарвлены мне.
    %% Отправителя и получателя надо указывать явно.
    %%
    ?evman_args([Hap], <<" = get message for me">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [nulluble, integer]
            },
            #norm_rule{
                key         = reader_nick,
                required    = false,
                types       = [nulluble, string]
            },
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            }
            | empweb_norm_doc:norm('get')
        ]),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:get_message_for_me(
                        [{pers_id, Pers_id}|Data#norm.return],
                        proplists:get_value(fields, Data#norm.return, [])
                    )
                ),
                Hap
            }
        end
    );


handle(_req, #empweb_hap{
        is_auth =   true,
        action  =   get_message_from_me,
        params  =   Params,
        pers_id =   Pers_id
    } = Hap) ->
    %%
    %% Pers_id при получении сообщения смысла не имеет.
    %% т.к. мы смысл имеет получать как своих сообщений,
    %% так и чужих, которые отпарвлены мне.
    %% Отправителя и получателя надо указывать явно.
    %%
    ?evman_args([Hap], <<" = get message from me">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [nulluble, integer]
            },
            #norm_rule{
                key         = reader_nick,
                required    = false,
                types       = [nulluble, string]
            },
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            }
            | empweb_norm_doc:norm('get')
        ]),
        fun(Data)->
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:get_message_from_me(
                        [{pers_id, Pers_id}|Data#norm.return],
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
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = reader_nick,
                required    = false,
                types       = [string]
            },
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            }
            | empweb_norm_doc:norm('create')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:create_message([
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

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = reader_nick,
                required    = false,
                types       = [string]
            },  
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            }
            | empweb_norm_doc:norm('update')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:update_message(Data#norm.return)
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
    ?evman_args([Hap], <<" = delete message for me">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_doc:norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:delete_message_for_me([{pers_id, Pers_id}|Data#norm.return])
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
    ?evman_args([Hap], <<" =  delete message from me">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, empweb_norm_doc:norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_doc:delete_message_from_me([{pers_id, Pers_id}|Data#norm.return])
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
%     empweb_jsonapi:handle_params(
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
%             | empweb_norm_doc:norm('update')
%         ]),
%         fun(Data)->
%             ?evman_debug(Data, <<" = Data">>),
%             {ok,
%                 empweb_jsonapi:resp(
%                     empweb_biz_doc:update_message([
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

    {ok,empweb_jsonapi:forbidden(), Hap}.


terminate(_req, Hap)->
    ?evman_args([Hap], <<" = terminate">>),
    ok.

%% ===========================================================================
%% Внутрениие функции
%% ===========================================================================

