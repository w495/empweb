%% @file    empdb_biz_flag.erl
%%          Описание бизнес логики работы с фотографиями.
%%          Фотография это просто документ.
%% 
-module(empdb_biz_flag).

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
        empdb_biz_doc:repost(empdb_dao_flag, Con, Params)
    end).

create(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_flag:create(Con, Params)
    end).

update(Params)->
    
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_flag:update(Con, Params)
    end).

get(Params)->
    nviewsup(fun empdb_dao_flag:update/2, [Params]),
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_flag:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    nviewsup(fun empdb_dao_flag:update/2, [Params]),
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_flag:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

nviewsup(Function, [Params]) when erlang:is_function(Function, 2)->
    spawn_link(fun()->
        empdb_dao:with_connection(fun(Con)->
            {ok, _} = Function(Con, [
                {filter, [{isdeleted, false}|Params]},
                {values, [{nviews, {incr, 1}}]}
            ])
        end)
    end);

nviewsup(Module, [Params])->
    nviewsup(Module, update, [Params]).

nviewsup(Module, Function, [Params])->
    spawn_link(fun()->
        empdb_dao:with_connection(fun(Con)->
            {ok, _} = Module:Function(Con, [
                {filter, [{isdeleted, false}|Params]},
                {values, [{nviews, {incr, 1}}]}
            ])
        end)
    end).


    
delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_flag:update(Con, [{isdeleted, true}|Params])
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_flag:is_owner(Con, Uid, Oid)
    end).
