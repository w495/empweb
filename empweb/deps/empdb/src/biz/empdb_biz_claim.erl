%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_claim).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").


%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Блоги
%%
-export([
    get/1,
    get/2,
    create/1,
    update/1
]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          ЗНАЧИМЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Блоги
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_claim:create(Con, Params)
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_claim:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_connection(fun(Con)->
        get_adds(
            Con,
            empdb_dao_claim:get(Con, [{isdeleted, false}|Params])
        )
    end).

get_adds(Con, {ok, Res}) ->
    {ok,
        lists:map(
            fun({Itempl})->
                {ok, [{Ownerpl}]} =
                    empdb_dao_pers:get(
                        Con,
                        [
                            {'or', [
                                {id,    proplists:get_value(owner_id,   Itempl)},
                                {nick,  proplists:get_value(owner_nick, Itempl)}
                            ]},
                            {fields, [authority_id, authority_alias]}
                        ]
                    ),
                {[
                    {pers_authority_id,
                        proplists:get_value(authority_id, Ownerpl)
                    },
                    {pers_authority_alias,
                        proplists:get_value(authority_alias, Ownerpl)
                    }
                    |Itempl
                ]}
            end,
            Res
        )
    };

get_adds(_Con, Else) ->
    Else.

get(Params, Fileds)->
    empdb_dao:with_connection(fun(Con)->
        get_adds(
            Con,
            empdb_dao_claim:get(Con, [{isdeleted, false}|Params], Fileds)
        )
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_claim:is_owner(Con, Uid, Oid)
    end).
