%%
%% @file    jsonapi_pers.erl
%%          "Контроллер" для функций работы с пользователями.
%%

-module(jsonapi_doc).
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
        pers_id         =   Pid,
        pers_perm_names =   Pns
    } = Hap)->
    %%%
    %%% Это нужно, чтобы понять, какая функция дальше выполнится
    %%%
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




handle(_req, #empweb_hap{action='get_all_oktypes'} = Hap) ->
    ?evman_args(Hap, <<" = get_oktypes">>),

    {ok,jsonapi:resp(biz_doc:get_oktype([])),Hap};


handle(_req, #empweb_hap{action='get_oktype', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = get_oktype">>),
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
            {ok,jsonapi:resp(biz_doc:get_oktype(Data#norm.return)),Hap}
        end,
        Hap
    );

handle(_req, #empweb_hap{action='get_all_mstatuses'} = Hap) ->
    ?evman_args(Hap, <<" = get_all_mstatuses">>),
    {ok,jsonapi:resp(biz_doc:get_mstatus([])),Hap};

handle(_req, #empweb_hap{action='get_mstatus', params=Params} = Hap) ->
    ?evman_args(Hap, <<" = get_mstatus">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = id,
                types = [integer]
            }
        ]),
        fun(Data)->
            {ok,jsonapi:resp(biz_doc:get_mstatus(Data#norm.return)),Hap}
        end,
        Hap
    );



handle(_req, #empweb_hap{action=get_all_chatlangs, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_all_chatlangs">>),

    {ok,jsonapi:resp(biz_doc:get_chatlang([])),Hap};

handle(_req, #empweb_hap{action='get_chatlang', params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_chatlang">>),

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
            {ok,jsonapi:resp(biz_doc:get_chatlang(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=create_chatlang, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = create_chatlang">>),
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
            {ok,jsonapi:resp(biz_doc:create_chatlang(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=update_chatlang, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = update_chatlang">>),

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
            {ok,jsonapi:resp(biz_doc:update_chatlang(Data#norm.return)),Hap}
        end
    );











handle(_req, #empweb_hap{action=get_all_doctypes, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_all_doctypes">>),

    {ok,jsonapi:resp(biz_doc:get_doctype([])),Hap};

handle(_req, #empweb_hap{action='get_doctype', params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_doctype">>),

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
            {ok,jsonapi:resp(biz_doc:get_doctype(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=create_doctype, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = create_doctype">>),
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
            {ok,jsonapi:resp(biz_doc:create_doctype(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=update_doctype, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = update_doctype">>),

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
            {ok,jsonapi:resp(biz_doc:update_doctype(Data#norm.return)),Hap}
        end
    );









handle(_req, #empweb_hap{action=get_all_contypes, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_all_contypes">>),

    {ok,jsonapi:resp(biz_doc:get_contype([])),Hap};

handle(_req, #empweb_hap{action='get_contype', params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_contype">>),

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
            {ok,jsonapi:resp(biz_doc:get_contype(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=create_contype, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = create_contype">>),
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
            {ok,jsonapi:resp(biz_doc:create_contype(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=update_contype, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = update_contype">>),

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
            {ok,jsonapi:resp(biz_doc:update_contype(Data#norm.return)),Hap}
        end
    );






handle(_req, #empweb_hap{action=get_all_acctypes, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_all_acctypes">>),

    {ok,jsonapi:resp(biz_doc:get_acctype([])),Hap};

handle(_req, #empweb_hap{action='get_acctype', params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_acctype">>),

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
            {ok,jsonapi:resp(biz_doc:get_acctype(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=create_acctype, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = create_acctype">>),
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
            {ok,jsonapi:resp(biz_doc:create_acctype(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=update_acctype, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = update_acctype">>),

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
            {ok,jsonapi:resp(biz_doc:update_acctype(Data#norm.return)),Hap}
        end
    );












handle(_req, #empweb_hap{action=get_all_roomtypes, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_all_roomtypes">>),

    {ok,jsonapi:resp(biz_doc:get_roomtype([])),Hap};

handle(_req, #empweb_hap{action='get_roomtype', params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_roomtype">>),

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
            {ok,jsonapi:resp(biz_doc:get_roomtype(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=create_roomtype, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = create_roomtype">>),
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
            {ok,jsonapi:resp(biz_doc:create_roomtype(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=update_roomtype, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = update_roomtype">>),

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
            {ok,jsonapi:resp(biz_doc:update_roomtype(Data#norm.return)),Hap}
        end
    );













handle(_req, #empweb_hap{action=get_all_communitytypes, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_all_communitytypes">>),

    {ok,jsonapi:resp(biz_doc:get_communitytype([])),Hap};

handle(_req, #empweb_hap{action='get_communitytype', params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_communitytype">>),

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
            {ok,jsonapi:resp(biz_doc:get_communitytype(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=create_communitytype, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = create_communitytype">>),
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
            {ok,jsonapi:resp(biz_doc:create_communitytype(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{action=update_communitytype, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = update_communitytype">>),

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
            {ok,jsonapi:resp(biz_doc:update_communitytype(Data#norm.return)),Hap}
        end
    );














handle(_req, #empweb_hap{action=get_all_blogs, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_all_blogs">>),

    {ok,jsonapi:resp(biz_doc:get_blog([{owner_id, Owner_id}])),Hap};

handle(_req, #empweb_hap{action='get_blog', params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_blog">>),

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
            {ok,jsonapi:resp(biz_doc:get_blog([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );

handle(_req, #empweb_hap{action=create_blog, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = create_blog">>),
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
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,jsonapi:resp(biz_doc:create_blog([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );

handle(_req, #empweb_hap{action=update_blog, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = update_blog">>),

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
            {ok,jsonapi:resp(biz_doc:update_blog([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );






handle(_req, #empweb_hap{action=get_all_rooms, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_all_rooms">>),

    {ok,jsonapi:resp(biz_doc:get_room([{owner_id, Owner_id}])),Hap};

handle(_req, #empweb_hap{action='get_room', params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_room">>),

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
            {ok,jsonapi:resp(biz_doc:get_room([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );

handle(_req, #empweb_hap{action=create_room, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = create_room">>),
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
            {ok,jsonapi:resp(biz_doc:create_room([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );

handle(_req, #empweb_hap{action=update_room, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = update_room">>),

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
            ?debug(" ---> biz_doc:update_room(~p) ~n", [[{owner_id, Owner_id}|Data#norm.return]]),
            
            {ok,jsonapi:resp(biz_doc:update_room([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );




handle(_req, #empweb_hap{action=get_all_communities, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_all_communities">>),

    {ok,jsonapi:resp(biz_doc:get_community([{owner_id, Owner_id}])),Hap};

handle(_req, #empweb_hap{action='get_community', params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_community">>),

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
            {ok,jsonapi:resp(biz_doc:get_community([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );

handle(_req, #empweb_hap{action=create_community, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = create_community">>),
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
            {ok,jsonapi:resp(biz_doc:create_community([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );

handle(_req, #empweb_hap{action=update_community, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = update_community">>),

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
            ?debug(" ---> biz_doc:update_community(~p) ~n", [[{owner_id, Owner_id}|Data#norm.return]]),

            {ok,jsonapi:resp(biz_doc:update_community([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );



handle(_req, #empweb_hap{action=get_all_posts, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_all_posts">>),

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
            {ok,jsonapi:resp(biz_doc:get_post([{owner_id, Owner_id}|Data#norm.return])),Hap}
            %{ok,jsonapi:resp(biz_doc:get_post([{owner_id, Owner_id}])),Hap};
        end
    );

handle(_req, #empweb_hap{action=get_post, params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = get_post">>),

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
            {ok,jsonapi:resp(biz_doc:get_post([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );

handle(_req, #empweb_hap{action=create_post, params=Params, pers_id=Owner_id} = Hap) ->
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
            ?evman_debug(Data,              <<" = create post data">>),
            ?evman_debug(Data#norm.return,  <<" = create post data#norm.return">>),

            ?debug("Data = ~p~n", [Data]),

            {ok,jsonapi:resp(biz_doc:create_post([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );

handle(_req, #empweb_hap{action='update_post', params=Params, pers_id=Owner_id} = Hap) ->
    ?evman_args([Hap], <<" = update_post">>),

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
            {ok,jsonapi:resp(biz_doc:update_post([{owner_id, Owner_id}|Data#norm.return])),Hap}
        end
    );




handle(_req, #empweb_hap{action=Action, params=Params, is_auth=Is_auth, pers_id=Pers_id} = Hap) ->
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

%% ---------------------------------------------------------------------------
%% Внутрениие функции
%% ---------------------------------------------------------------------------

at_list_one([]) ->
    {error, no_param};
at_list_one([Return|_]) ->
    {ok, Return}.

