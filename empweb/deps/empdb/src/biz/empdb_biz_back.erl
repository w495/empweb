%% @file    empdb_biz_back.erl
%%          Описание бизнес логики работы с фотографиями.
%%          Фотография это просто документ.
%% 
-module(empdb_biz_back).

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
    update/1,
    repost/1
]).


repost(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_biz_doc:repost(empdb_dao_back, Con, Params)
    end).

create(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_back:create(Con, Params)
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_back:update(Con, Params)
    end).

get(Params)->
    empdb_biz:nviewsupm(?MODULE, [Params]),
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_back:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_biz:nviewsupm(?MODULE, [Params]),
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_back:get(Con, [{isdeleted, false}|Params], Fileds)
    end).


    
delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_back:update(Con, [{isdeleted, true}|Params])
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_back:is_owner(Con, Uid, Oid)
    end).
