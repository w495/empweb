%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add descr to biz_user
-module(empdb_dao_experbuy).
-behaviour(empdb_dao).

%%
%% Include files
%%


%%
%% Exported Functions
%%

-export([
    table/1,
    table/0,
    create/2,
    update/2,
    get/2,
    get/3,
    is_owner/3
]).


%%
%% API Functions
%%

%%
%% @doc Возвращает список обязательных полей таблицы для создания
%%
table({fields, insert, required})-> [];

%%
%% @doc Возвращает список полей таблицы для выборки
%%
table({fields, select})->
    table({fields, all}) -- [isdeleted];

%%
%% @doc Возвращает список полей таблицы для обновления
%%
table({fields, update})->
    table({fields, all}) -- [id];

%%
%% @doc Возвращает список полей таблицы для создания
%%
table({fields, insert})->
    table({fields, all}) -- [id];

%%
%% @doc Возвращает полный список полей таблицы
%%
table({fields, all})->
    [
        id,
        buyer_id,
        buyer_nick,
        owner_id,
        owner_nick,
        exper,
        price,
        created,
        isforwish,
        isdeleted
    ];

%%
%% @doc Возвращает полный список полей таблицы
%%
table(fields)->
    table({fields, all});

%%
%% @doc Возвращает имя таблицы
%%
table(name)->
    experbuy.

table()->
    table(name).

get(Con, What) ->
    empdb_dao:get(?MODULE, Con, What).

get(Con, What, Fields)->
    empdb_dao:get(?MODULE, Con, What, Fields).

create(Con, Proplist)->
    empdb_dao:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    empdb_dao:update(?MODULE, Con, Proplist).

is_owner(Con, Id, Id)->
    true;
is_owner(Con, _, _)->
    false.


%%
%% Local Functions
%%

