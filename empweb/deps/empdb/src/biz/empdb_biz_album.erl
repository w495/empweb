%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_album).

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
    delete/1,
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
        empdb_dao_album:create(Con, Params)
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_album:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_album:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_album:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_album:update(Con, [{isdeleted, true}|Params])
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_album:is_owner(Con, Uid, Oid)
    end).
