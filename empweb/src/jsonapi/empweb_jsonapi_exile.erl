%%
%% @file    empweb_jsonapi_pers.erl
%%          "Контроллер" для функций работы с системными настройками,
%%          языками, и связанными с ними объектами.
%%

-module(empweb_jsonapi_exile).
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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Языки
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%         id,
%         roomlot_id,
%         room_id,
%         room_head,
%         owner_id,
%         owner_nick
%

handle(_req, #empweb_hap{
        action='get', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get exile">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = pers_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = pers_nick,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = sender_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = sender_nick,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = savior_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = savior_nick,
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
                key         = roomtype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = roomtype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = reason,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = expired,
                required    = false,
                types       = empweb_norm:filter([unixdatetime])
            },
            #norm_rule{
                key         = created,
                required    = false,
                types       = empweb_norm:filter([unixdatetime])
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_exile:get(Data#norm.return)
                ),
                Hap
            }
        end
    );



handle(_req, #empweb_hap{
        action='count', params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = count exile">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = empweb_norm:filter([integer])
            },
            #norm_rule{
                key         = pers_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = pers_nick,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = sender_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = sender_nick,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = savior_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = savior_nick,
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
                key         = roomtype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = roomtype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = reason,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = expired,
                required    = false,
                types       = empweb_norm:filter([unixdatetime])
            },
            #norm_rule{
                key         = created,
                required    = false,
                types       = empweb_norm:filter([unixdatetime])
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_exile:count(Data#norm.return)
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create, params=Params, pers_id=Sender_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create exile">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = pers_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = pers_nick,
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
                key         = reason,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = expired,
                required    = false,
                types       = [unixdatetime]
            },
            #norm_rule{
                key         = created,
                required    = false,
                types       = [unixdatetime]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_exile:create([
                        {sender_id, Sender_id}
                        | Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=update, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update exile">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = pers_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = pers_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = sender_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = sender_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = savior_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = savior_nick,
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
                key         = roomtype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = roomtype_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = reason,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = expired,
                required    = false,
                types       = [unixdatetime]
            },
            #norm_rule{
                key         = created,
                required    = false,
                types       = [unixdatetime]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_exile:update(Data#norm.return)
                ),
                Hap
            }
        end
    );


handle(_req, #empweb_hap{
        action=delete, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = delete exile">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = pers_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = pers_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = sender_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = sender_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = savior_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = savior_nick,
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
                key         = roomtype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = roomtype_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = reason,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = expired,
                required    = false,
                types       = [unixdatetime]
            },
            #norm_rule{
                key         = created,
                required    = false,
                types       = [unixdatetime]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_exile:delete([
                        {self@pers_id, Pers_id}
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
