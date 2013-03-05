%%
%% @file    empweb_jsonapi_pers.erl
%%          "Контроллер" для функций работы с системными настройками,
%%          языками, и связанными с ними объектами.
%%

-module(empweb_jsonapi_event).
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
    } = _Hap)->
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


handle(_req, #empweb_hap{
        action  =   'count',
        params  =   Params,
        pers_id =   Pers_id
        
    } = Hap) ->
    ?evman_args([Hap], <<" = get event">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = head,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = body,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
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
                key         = friendtype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = friendtype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = eventtype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = eventtype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },

            #norm_rule{
                key         = doc_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = doc_head,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = doc_owner_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = doc_owner_nick,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = doc_parent_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = doctype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = doctype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = orig_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = orig_owner_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = orig_owner_nick,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = isnews,
                required    = false,
                types       = empweb_norm:filter([nullable, boolean])
            },
            #norm_rule{
                key         = eventobj_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = eventobj_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = eventact_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = eventact_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = eventspc_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = eventspc_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = target_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            }
            |empweb_norm:norm('get')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_event:count(
                        empweb_norm:fieldtrigger([
                            {'self@pers_id', Pers_id}
                            |Data#norm.return
                        ], {owner_id, 'self@pers_id'})
                    )
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action='get',
        params=Params,
        pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get event">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = head,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = body,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
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
                key         = friendtype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = friendtype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = eventtype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = eventtype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = doc_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = doc_head,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = doc_owner_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = doc_owner_nick,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = doc_parent_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = doctype_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = doctype_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = orig_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = orig_owner_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = orig_owner_nick,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = isnews,
                required    = false,
                types       = empweb_norm:filter([nullable, boolean])
            },
            #norm_rule{
                key         = eventobj_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = eventobj_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = eventact_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = eventact_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = eventspc_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            },
            #norm_rule{
                key         = eventspc_alias,
                required    = false,
                types       = empweb_norm:filter([nullable, string])
            },
            #norm_rule{
                key         = target_id,
                required    = false,
                types       = empweb_norm:filter([nullable, integer])
            }
            |empweb_norm:norm('get')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_event:get(
                        empweb_norm:fieldtrigger([
                            {'self@pers_id', Pers_id}
                            |Data#norm.return
                        ], {owner_id, 'self@pers_id'})
                    )
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=create, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create event">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
                        #norm_rule{
                key         = id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = head,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = body,
                required    = false,
                types       = [nullable, string]
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
                key         = friendtype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = friendtype_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = eventtype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventtype_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = doc_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doc_head,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = doc_owner_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doc_owner_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = doc_parent_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doctype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doctype_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = orig_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = orig_owner_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = orig_owner_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = isnews,
                required    = false,
                types       = [nullable, boolean]
            },
            #norm_rule{
                key         = eventobj_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventobj_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = eventact_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventact_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = eventspc_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventspc_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = target_id,
                required    = false,
                types       = [nullable, integer]
            }
            |empweb_norm:norm('create')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_event:create([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=update, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update event">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
                       #norm_rule{
                key         = id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = head,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = body,
                required    = false,
                types       = [nullable, string]
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
                key         = friendtype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = friendtype_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = eventtype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventtype_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = doc_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doc_head,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = doc_owner_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doc_owner_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = doc_parent_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doctype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doctype_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = orig_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = orig_owner_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = orig_owner_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = isnews,
                required    = false,
                types       = [nullable, boolean]
            },
            #norm_rule{
                key         = eventobj_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventobj_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = eventact_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventact_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = eventspc_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventspc_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = target_id,
                required    = false,
                types       = [nullable, integer]
            }
            |empweb_norm:norm('update')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_event:update([
                        {owner_id, Pers_id}
                        |Data#norm.return
                    ])
                ),
                Hap
            }
        end
    );

handle(_req, #empweb_hap{
        action=delete, params=Params, pers_id=Pers_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update event">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
                        #norm_rule{
                key         = id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = head,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = body,
                required    = false,
                types       = [nullable, string]
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
                key         = friendtype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = friendtype_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = eventtype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventtype_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = doc_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doc_head,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = doc_owner_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doc_owner_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = doc_parent_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doctype_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = doctype_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = orig_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = orig_owner_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = orig_owner_nick,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = isnews,
                required    = false,
                types       = [nullable, boolean]
            },
            #norm_rule{
                key         = eventobj_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventobj_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = eventact_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventact_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = eventspc_id,
                required    = false,
                types       = [nullable, integer]
            },
            #norm_rule{
                key         = eventspc_alias,
                required    = false,
                types       = [nullable, string]
            },
            #norm_rule{
                key         = target_id,
                required    = false,
                types       = [nullable, integer]
            }
            |empweb_norm:norm('update')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,
                empweb_jsonapi:resp(
                    empweb_biz_event:delete([
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
    {ok,empweb_jsonapi:forbidden(), Hap}.


terminate(_req, Hap)->
    ?evman_args([Hap], <<" = terminate">>),
    ok.
