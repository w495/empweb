%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_thing).

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
    update/1,
    delete/1
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
        empdb_dao_thing:create(Con, Params)
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_thing:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_thing:get(Con, [{isdeleted, false}|Params])
    end).


count(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_thing:count(Con, [{isdeleted, false}|Params])
    end).


count(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_thing:get(Con, [{isdeleted, false}|Params])
    end).


get(Params, Fileds)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_thing:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

delete(Filter)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thing:update(Con, [
            {filter, [
                {isdeleted, false}
                |Filter
            ]},
            {values, [
                {isdeleted, true}
            ]}
        ])
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_thing:is_owner(Con, Uid, Oid)
    end).
