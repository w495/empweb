%%
%% @file    jsonapi_pers.erl
%%          "Контроллер" для функций работы с системными настройками,
%%          языками, и связанными с ними объектами.
%%

-module(jsonapi_conf).
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
        action=get_all_langs, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all langs">>),
    {ok,jsonapi:resp(biz_conf:get_lang([])),Hap};

handle(_req, #empweb_hap{
        action='get_lang', params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get lang">>),

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
            {ok,jsonapi:resp(biz_conf:get_lang(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{
        action=create_lang, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create lang">>),
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
            {ok,jsonapi:resp(biz_conf:create_lang(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{
        action=update_lang, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update lang">>),

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
            {ok,jsonapi:resp(biz_conf:create_lang(Data#norm.return)),Hap}
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Переводы
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle(_req, #empweb_hap{
        action=get_all_trs, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get all trs">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = lang_id,
                required    = false,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,jsonapi:resp(biz_conf:get_tr(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{
        action='get_tr', params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get tr">>),
    jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key = ti,
                types = [string]
            },
            #norm_rule{
                key = lang_id,
                types = [integer]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,jsonapi:resp(biz_conf:get_tr(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{
        action=create_tr, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create tr">>),
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
            {ok,jsonapi:resp(biz_conf:create_tr(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{
        action=update_tr, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update tr">>),

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
            {ok,jsonapi:resp(biz_conf:create_tr(Data#norm.return)),Hap}
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
