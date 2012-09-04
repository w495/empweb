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
    } = Hap)->
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

handle(_req, #empweb_hap{action='get_all_oktypes'} = Hap) ->
    ?evman_args(Hap, <<" = get oktypes">>),
    {ok,
        jsonapi:resp(biz_doc:get_oktype([])),
        Hap
    };

handle(_req, #empweb_hap{action='get_oktype', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = get oktype">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_one{
                rules=[
                    #norm_rule{
                        key = alias,
                        types = [string]
                    },
                    #norm_rule{
                        key = id,
                        types = [integer]
                    }
                ]
            }
        ]),
        fun(Data)->
            {ok,
                jsonapi:resp(biz_doc:get_oktype(Data#norm.return)),
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
        action=get_all_doctypes, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all doc types">>),
    {ok,jsonapi:resp(biz_doc:get_doctype([])),Hap};

handle(_req, #empweb_hap{
        action='get_doctype', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get doc type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_at_least_one{
                rules=[
                    #norm_rule{
                        key = id,
                        types = [integer]
                    },
                    #norm_rule{
                        key = alias,
                        types = [string]
                    }
                ]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_doctype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_doctype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create doc type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            },
            #norm_rule{
                key         = descr,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_doctype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=update_doctype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update doc type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            },
            #norm_rule{
                key         = descr,
                types       = [string]
            }
        ]),
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
        action=get_all_contypes, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all contypes">>),
    {ok,jsonapi:resp(biz_doc:get_contype([])),Hap};

handle(_req, #empweb_hap{
        action='get_contype', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get contype">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_at_least_one{
                rules=[
                    #norm_rule{
                        key = id,
                        types = [integer]
                    },
                    #norm_rule{
                        key = alias,
                        types = [string]
                    }
                ]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_contype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_contype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create contype">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            },
            #norm_rule{
                key         = descr,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_contype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=update_contype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update contype">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            },
            #norm_rule{
                key         = descr,
                types       = [string]
            }
        ]),
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
        action=get_all_acctypes, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all acctypes">>),
    {ok,jsonapi:resp(biz_doc:get_acctype([])),Hap};

handle(_req, #empweb_hap{
        action='get_acctype', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get acctype">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_at_least_one{
                rules=[
                    #norm_rule{
                        key = id,
                        types = [integer]
                    },
                    #norm_rule{
                        key = alias,
                        types = [string]
                    }
                ]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_acctype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_acctype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create acctype">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            },
            #norm_rule{
                key         = descr,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_acctype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=update_acctype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update acctype">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            },
            #norm_rule{
                key         = descr,
                types       = [string]
            }
        ]),
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
        action=get_all_roomtypes, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all room types">>),
    {ok,jsonapi:resp(biz_doc:get_roomtype([])),Hap};

handle(_req, #empweb_hap{
        action='get_roomtype', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get room type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_at_least_one{
                rules=[
                    #norm_rule{
                        key = id,
                        types = [integer]
                    },
                    #norm_rule{
                        key = alias,
                        types = [string]
                    }
                ]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_roomtype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_roomtype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create room type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_roomtype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=update_roomtype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update room type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            }
        ]),
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
        action=get_all_chatlangs, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all chatlangs">>),
    {ok,jsonapi:resp(biz_doc:get_chatlang([])),Hap};

handle(_req, #empweb_hap{
        action='get_chatlang', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get chatlang">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_at_least_one{
                rules=[
                    #norm_rule{
                        key = id,
                        types = [integer]
                    },
                    #norm_rule{
                        key = alias,
                        types = [string]
                    }
                ]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_chatlang(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_chatlang, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create chatlang">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            },
            #norm_rule{
                key         = descr,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_chatlang(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=update_chatlang, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update chatlang">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            },
            #norm_rule{
                key         = descr,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:update_chatlang(Data#norm.return)),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Список режимов страны
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        action=get_all_regimens, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all regimens">>),

    {ok,jsonapi:resp(biz_doc:get_regimen([])),Hap};

handle(_req, #empweb_hap{
        action='get_regimen', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get regimen">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_at_least_one{
                rules=[
                    #norm_rule{
                        key = id,
                        types = [integer]
                    },
                    #norm_rule{
                        key = alias,
                        types = [string]
                    }
                ]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_regimen(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_regimen, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create regimen">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_regimen(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=update_regimen, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update regimen">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:update_regimen(Data#norm.return)),
                Hap
            }
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы сообществ (обычные, тайные)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        action=get_all_communitytypes, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all community types">>),
    {ok,jsonapi:resp(biz_doc:get_communitytype([])),Hap};

handle(_req, #empweb_hap{
        action='get_communitytype', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get community type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_at_least_one{
                rules=[
                    #norm_rule{
                        key = id,
                        types = [integer]
                    },
                    #norm_rule{
                        key = alias,
                        types = [string]
                    }
                ]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_communitytype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_communitytype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create community type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_communitytype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=update_communitytype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update community type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            }
        ]),
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
        action=get_all_messagetypes, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all message types">>),
    {ok,jsonapi:resp(biz_doc:get_messagetype([])),Hap};

handle(_req, #empweb_hap{
        action='get_messagetype', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get message type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_at_least_one{
                rules=[
                    #norm_rule{
                        key = id,
                        types = [integer]
                    },
                    #norm_rule{
                        key = alias,
                        types = [string]
                    }
                ]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:get_messagetype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_messagetype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create message type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(biz_doc:create_messagetype(Data#norm.return)),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=update_messagetype, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update message type">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                types       = [string]
            }
        ]),
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
        action='get_blog', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get blog[s]">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, doc_norm('get')),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:get_blog(filter_owner([
                        {pers_id, Pers_id}
                        |Data#norm.return
                    ]))
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_blog, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create blog">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
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
        action=update_blog, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update blog">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
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
        action=get_all_posts, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all posts">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = parent_id,
                required    = false,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:get_post([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=get_post, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get post">>),

    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                required    = false,
                types       = [integer]
            }
            #norm_rule{
                key         = parent_id,
                required    = false,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:get_post([
                        {pers_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_post, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create post">>),
    ?debug("Params = ~p~n", [Params]),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = head,
                types       = [string]
            },
            #norm_rule{
                key         = body,
                types       = [string]
            },
            #norm_rule{
                key         = parent_id,
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
                key         = contype_id,
                required    = false,
                types       = [integer]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data,
                <<" = create post data">>),
            ?evman_debug(Data#norm.return,
                <<" = create post data#norm.return">>),
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
        action='update_post', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update post">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = head,
                types       = [string]
            },
            #norm_rule{
                key         = body,
                types       = [string]
            },
            #norm_rule{
                key         = parent_id,
                required    = false,
                types       = [integer]
            }
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
            }
        ]),
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
%% Чат-комнаты (страны)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        action=get_all_rooms, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all rooms">>),
    {ok,jsonapi:resp(biz_doc:get_room([{pers_id, Pers_id}])),Hap};

handle(_req, #empweb_hap{
        action='get_room', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get room">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                types = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:get_room([
                        {pers_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_room, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create room">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = head,
                types = [string]
            },
            #norm_rule{
                key = body,
                types = [string]
            },
            #norm_rule{
                key         = parent_id,
                required    = false,
                types       = [integer]
            }
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
        action=update_room, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update room">>),

    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key = head,
                types = [string]
            },
            #norm_rule{
                key = body,
                types = [string]
            },
            #norm_rule{
                key         = parent_id,
                required    = false,
                types       = [integer]
            }
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
        action=get_all_communities, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all communities">>),
    {ok,jsonapi:resp(biz_doc:get_community([{pers_id, Pers_id}])),Hap};

handle(_req, #empweb_hap{
        action='get_community', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get community">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                types = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:get_community([
                        {pers_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_community, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create community">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = head,
                types = [string]
            },
            #norm_rule{
                key = body,
                types = [string]
            },
            #norm_rule{
                key         = parent_id,
                required    = false,
                types       = [integer]
            }
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
        action=update_community, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update community">>),

    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key = head,
                types = [string]
            },
            #norm_rule{
                key = body,
                types = [string]
            },
            #norm_rule{
                key         = parent_id,
                required    = false,
                types       = [integer]
            }
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
%% Сообщества
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle(_req, #empweb_hap{
        action=get_all_messages, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all messages">>),
    {ok,jsonapi:resp(biz_doc:get_message([{pers_id, Pers_id}])),Hap};

handle(_req, #empweb_hap{
        action='get_message', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get message">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = parent_id,
                required    = false,
                types       = [nulluble, integer]
            }
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            },
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [nulluble, integer]
            }
            #norm_rule{
                key         = oktype_id,
                required    = false,
                types       = [nulluble, integer]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:get_message([
                        {pers_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create_message, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create message">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = head,
                types = [string]
            },
            #norm_rule{
                key = body,
                types = [string]
            },
            #norm_rule{
                key         = parent_id,
                required    = false,
                types       = [nulluble, integer]
            }
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            },
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [nulluble, integer]
            }
            #norm_rule{
                key         = oktype_id,
                required    = false,
                types       = [nulluble, integer]
            }
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
        action=update_message, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update message">>),

    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key = head,
                types = [string]
            },
            #norm_rule{
                key = body,
                types = [string]
            },
            #norm_rule{
                key         = parent_id,
                required    = false,
                types       = [nulluble, integer]
            }
            #norm_rule{
                key         = type_id,
                required    = false,
                types       = [nulluble, integer]
            },
            #norm_rule{
                key         = reader_id,
                required    = false,
                types       = [nulluble, integer]
            }
            #norm_rule{
                key         = oktype_id,
                required    = false,
                types       = [nulluble, integer]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                jsonapi:resp(
                    biz_doc:update_message([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );








handle(_req, #empweb_hap{
        action=Action, params=Params, is_auth=Is_auth, pers_id=Pers_id
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
        {undefined, Pers_id     } ->
            [{owner_id, Pers_id}|proplists:delete(owner_id, Params)];
        _ ->
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
            types = [integer]
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
            key = owner_id,
            required = false,
            types = [nullable, integer]
        }
        | jsonapi:norm('get')
    ];

doc_norm('create') ->
    [
        #norm_rule{
            key         = head,
            types       = [string]
        },
        #norm_rule{
            key         = body,
            types       = [string]
        },
        #norm_rule{
            key         = parent_id,
            required    = false,
            types       = [integer]
        }
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

doc_norm(X) ->
    [].