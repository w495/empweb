%% @file    empdb_biz_photo.erl
%%          Описание бизнес логики работы с фотографиями.
%%          Фотография это просто документ.
%% 
-module(empdb_biz_photo).

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
        empdb_biz_doc:repost(empdb_dao_photo, Con, Params)
    end).

create(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_photo:create(Con, Params)
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_photo:update(Con, Params)
    end).

get(Params)->
    empdb_biz:nviewsupm(?MODULE, [Params]),
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_photo:get_adds(Con,
            empdb_dao_photo:get(Con, [{isdeleted, false}|Params]),
            Params
        )
    end).

get(Params, Fields)->
    empdb_biz:nviewsupm(?MODULE, [Params]),
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_photo:get_adds(Con,
            empdb_dao_photo:get(Con, [{isdeleted, false}|Params], Fields),
            [{fields, Fields}|Params]
        )
    end).


    
delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_photo:update(Con, [{isdeleted, true}|Params])
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_photo:is_owner(Con, Uid, Oid)
    end).
