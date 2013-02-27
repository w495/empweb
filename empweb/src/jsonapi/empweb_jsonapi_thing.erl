%%
%% @file    empweb_jsonapi_pers.erl
%%          "Контроллер" для функций работы с системными настройками,
%%          языками, и связанными с ними объектами.
%%

-module(empweb_jsonapi_thing).
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


handle(_req, #empweb_hap{
        action='get', params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get thing">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                required    = false,
                types       = [string]
            },
            #norm_rule{
                key         = price,
                required    = false,
                types       = [float]
            },
            #norm_rule{
                key         = rent,
                required    = false,
                types       = [float]
            },
            #norm_rule{
                key         = thingtype_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = thingtype_alias,
                required    = false,
                types       = [string]
            },
            #norm_rule{
                key         = file_id,
                required    = false,
                types       = [integer]
            }
            |empweb_norm:norm('get')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,empweb_jsonapi:resp(empweb_biz_thing:get(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{
        action=create, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create thing">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = alias,
                required    = false,
                types       = [string]
            },
            #norm_rule{
                key         = name_ti,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = descr_ti,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = price,
                required    = false,
                types       = [float]
            },
            #norm_rule{
                key         = rent,
                required    = false,
                types       = [float]
            },
            #norm_rule{
                key         = thingtype_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = thingtype_alias,
                required    = false,
                types       = [string]
            },
            #norm_rule{
                key         = file_id,
                required    = false,
                types       = [integer]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,empweb_jsonapi:resp(empweb_biz_thing:create(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{
        action=update, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update thing">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                required    = false,
                types       = [string]
            },
            #norm_rule{
                key         = name_ti,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = descr_ti,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = price,
                required    = false,
                types       = [float]
            },
            #norm_rule{
                key         = rent,
                required    = false,
                types       = [float]
            },
            #norm_rule{
                key         = thingtype_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = thingtype_alias,
                required    = false,
                types       = [string]
            },
            #norm_rule{
                key         = file_id,
                required    = false,
                types       = [integer]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,empweb_jsonapi:resp(empweb_biz_thing:update(Data#norm.return)),Hap}
        end
    );



handle(_req, #empweb_hap{
        action=delete, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = delete thing">>),

    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = alias,
                required    = false,
                types       = [string]
            },
            #norm_rule{
                key         = name_ti,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = descr_ti,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = price,
                required    = false,
                types       = [float]
            },
            #norm_rule{
                key         = rent,
                required    = false,
                types       = [float]
            },
            #norm_rule{
                key         = thingtype_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = thingtype_alias,
                required    = false,
                types       = [string]
            },
            #norm_rule{
                key         = file_id,
                required    = false,
                types       = [integer]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,empweb_jsonapi:resp(empweb_biz_thing:delete(Data#norm.return)),Hap}
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
