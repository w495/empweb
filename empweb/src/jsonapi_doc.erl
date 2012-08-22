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
                key         = contype_id,
                required    = false,
                types       = [integer]
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
            {ok,jsonapi:resp(biz_doc:update_room([{owner_id, Owner_id}|Data#norm.return])),Hap}
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

