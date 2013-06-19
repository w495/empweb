%%
%% @file    empweb_jsonapi_pers.erl
%%          "Контроллер" для функций работы с системными настройками,
%%          языками, и связанными с ними объектами.
%%

-module(empweb_jsonapi_thingbuy).
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
        pers_nick       =   Pnick,
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
        {pers_nick,         Pnick},
        {pers_perm_names,   Pns}
    ]}, <<" = Hap">>),

    {ok,
        Req,
        #empweb_hap{
            action          =   Action,
            params          =   Params,
            is_auth         =   Is_auth,
            pers_id         =   Pid,
            pers_nick       =   Pnick,
            pers_perm_names =   Pns
        }
    }.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Языки
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



handle(_req, #empweb_hap{
        action      =   count,
        params      =   Params,
        pers_id     =   Pers_id,
        pers_nick   =   Pers_nick
    } = Hap) ->
    ?evman_args([Hap], <<" = get buy">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, begin
        X = [
            #norm_rule{
                key         = id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = owner_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = owner_nick,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = buyer_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = buyer_nick,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = room_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = room_head,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = community_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = community_head,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = price,
                required    = false,
                types       = empweb_norm:filter([nullable, float])
            },
            #norm_rule{
                key         = rent,
                required    = false,
                types       = empweb_norm:filter([nullable, float])
            },
            #norm_rule{
                key         = costs,
                required    = false,
                types       = empweb_norm:filter([nullable, float])
            },
            #norm_rule{
                key         = expired,
                required    = false,
                types       = empweb_norm:filter([nullable, unixdatetime])
            },
            #norm_rule{
                key         = thing_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = thing_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = thingtype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = thingtype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = image_width,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
                %default     = null
            },
            #norm_rule{
                key         = image_height,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
                %default     = null
            }
            |empweb_norm:norm('get')
        ],
        io:format("~n ~n ~n X  = ~p ~n ~n ~n ", [{X}]),
        X
        end),
        fun(Data)->
            io:format("~n~n~n Data#norm.return = ~p ~n~n~n", [Data#norm.return]),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_thingbuy:count(Data#norm.return)
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action      =   'get',
        params      =   Params,
        pers_id     =   Pers_id,
        pers_nick   =   Pers_nick
    } = Hap) ->
    ?evman_args([Hap], <<" = get buy">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = owner_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = owner_nick,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = room_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = room_head,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = community_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = community_head,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = price,
                required    = false,
                types       = empweb_norm:filter([nullable, float])
            },
            #norm_rule{
                key         = costs,
                required    = false,
                types       = empweb_norm:filter([nullable, float])
            },
            #norm_rule{
                key         = rent,
                required    = false,
                types       = empweb_norm:filter([nullable, float])
            },
            #norm_rule{
                key         = thing_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = expired,
                required    = false,
                types       = empweb_norm:filter([nullable, unixdatetime])
            },
            #norm_rule{
                key         = thing_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = thingtype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = thingtype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = image_width,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
                %default     = null
            },
            #norm_rule{
                key         = image_height,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
                %default     = null
            }
            |empweb_norm:norm('get')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),

            io:format("~n~n~n Data#norm.return = ~p ~n~n~n", [Data#norm.return]),

            io:format("~n~n~n Params = ~p ~n~n~n", [Params]),



            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_thingbuy:get(Data#norm.return)
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action      =   create,
        params      =   Params,
        pers_id     =   Pers_id,
        pers_nick   =   Pers_nick
    } = Hap) ->
    ?evman_args([Hap], <<" = create buy">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = owner_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = owner_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = room_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = room_head,
                required    = false,
                types       = [nullable, string]
            },

            #norm_rule{
                key         = community_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = community_head,
                required    = false,
                types       = [nullable, string]
            },

            #norm_rule{
                key         = price,
                required    = false,
                types       = [nullable, float]
            },
            #norm_rule{
                key         = rent,
                required    = false,
                types       = [nullable, float]
            },
            #norm_rule{
                key         = thing_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = thing_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = expired,
                required    = false,
                types       = [nullable, unixdatetime]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_thingbuy:create([
                        {buyer_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action      =   update,
        params      =   Params,
        pers_id     =   Pers_id,
        pers_nick   =   Pers_nick
    } = Hap) ->
    ?evman_args([Hap], <<" = update buy">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = owner_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = owner_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = room_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = room_head,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = thing_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = thing_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = expired,
                required    = false,
                types       = [nullable, unixdatetime]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_thingbuy:update([
                        {pers_id@check, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );


handle(_req, #empweb_hap{
        action      =   delete,
        params      =   Params,
        pers_id     =   Pers_id,
        pers_nick   =   Pers_nick
    } = Hap) ->
    ?evman_args([Hap], <<" = update buy">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = owner_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = owner_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = thing_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = room_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = room_head,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = thing_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = expired,
                required    = false,
                types       = [unixdatetime],
                default     = null
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_thingbuy:delete([
                        {buyer_id, Pers_id}
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
    {ok,empweb_jsonapi:forbidden(), Hap}.


terminate(_req, Hap)->
    ?evman_args([Hap], <<" = terminate">>),
    ok.
