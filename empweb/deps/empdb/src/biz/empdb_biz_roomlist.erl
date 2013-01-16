%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_roomlist).

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
    count/1,
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
        empdb_dao_roomlist:create(Con, Params)
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roomlist:update(Con, Params)
    end).

count(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roomlist:count(Con, [{isdeleted, false}|Params])
    end).
    
get(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roomlist:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roomlist:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

delete(Params)->
    empdb_dao:with_connection(fun(Con)->
        case proplists:get_value(pers_id, Params) of
            undefined ->
                empdb_dao_roomlist:delete(Con, Params);
            Pers_id ->
                empdb_dao_roomlist:delete(Con, Params)
        end
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roomlist:is_owner(Con, Uid, Oid)
    end).
