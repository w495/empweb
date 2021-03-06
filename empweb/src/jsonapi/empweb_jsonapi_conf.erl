%%
%% @file    empweb_jsonapi_pers.erl
%%          "Контроллер" для функций работы с системными настройками,
%%          языками, и связанными с ними объектами.
%%

-module(empweb_jsonapi_conf).
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
% handle(_req, #empweb_hap{
%         action=get_all_langs, params=Params, pers_id=Owner_id
%     } = Hap) ->
%     ?evman_args([Hap], <<" = get all langs">>),
%     {ok,empweb_jsonapi:resp(empweb_biz_conf:get_lang([])),Hap};

handle(_req, #empweb_hap{
        action='get_lang', params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get lang">>),

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
                key         = descr,
                required    = false,
                types       = [string]
            }
            |empweb_norm:norm('get')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,empweb_jsonapi:resp(empweb_biz_conf:get_lang(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{
        action=create_lang, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create lang">>),
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
                key         = descr,
                required    = false,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,empweb_jsonapi:resp(empweb_biz_conf:create_lang(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{
        action=update_lang, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update lang">>),

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
                key         = descr,
                required    = false,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,empweb_jsonapi:resp(empweb_biz_conf:create_lang(Data#norm.return)),Hap}
        end
    );

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Переводы
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% handle(_req, #empweb_hap{
%         action=get_all_trs, params=Params, pers_id=Owner_id
%     } = Hap) ->
%     ?evman_args([Hap], <<" = get all trs">>),
%     empweb_jsonapi:handle_params(
%         %% проверка входных параметров и приведение к нужному типу
%         norm:norm(Params, [
%             #norm_rule{
%                 key         = lang_id,
%                 required    = false,
%                 types       = [string]
%             }
%         ]),
%         fun(Data)->
%             ?evman_debug(Data, <<" = Data">>),
%             {ok,empweb_jsonapi:resp(empweb_biz_conf:get_tr(Data#norm.return)),Hap}
%         end
%     );

handle(_req, #empweb_hap{
        action='get_tr', params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = get tr">>),
    empweb_jsonapi:handle_params(
        %% проверка входных параметров и приведение к нужному типу
        norm:norm(Params, [
            #norm_rule{
                key         = tt,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = tf,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = ta,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = ti,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = lang_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = lang_alias,
                required    = false,
                types       = [string]
            },
            #norm_rule{
                key         = trtype_id,
                required    = false,
                types       = [integer]
            },
            #norm_rule{
                key         = trtype_alias,
                required    = false,
                types       = [string]
            },
            #norm_rule{
                key         = text,
                required    = false,
                types       = [string]
            }
            |empweb_norm:norm('get')
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,empweb_jsonapi:resp(empweb_biz_conf:get_tr(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{
        action=create_tr, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = create tr">>),
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
                types       = [string]
            },
            #norm_rule{
                key         = descr,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,empweb_jsonapi:resp(empweb_biz_conf:create_tr(Data#norm.return)),Hap}
        end
    );

handle(_req, #empweb_hap{
        action=update_tr, params=Params, pers_id=Owner_id
    } = Hap) ->
    ?evman_args([Hap], <<" = update tr">>),

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
                types       = [string]
            },
            #norm_rule{
                key         = descr,
                types       = [string]
            }
        ]),
        fun(Data)->
            ?evman_debug(Data, <<" = Data">>),
            {ok,empweb_jsonapi:resp(empweb_biz_conf:create_tr(Data#norm.return)),Hap}
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
