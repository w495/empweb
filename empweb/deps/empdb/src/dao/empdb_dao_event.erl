%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_dao_event).
-behaviour(empdb_dao).

%%
%% Include files
%%


%%
%% Exported Functions
%%
-export([
    count/2,
    table/1,
    table/0,
    create/2,
    update/2,
    get/2,
    get/3
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
    table({fields, all});

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
        doc_id,
        eventtype_id,
        eventtype_alias,
        own_community_id,
        pers_id,
        pers_nick
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
    event.

table()->
    table(name).


count(Con, What) ->
    empdb_dao_doc:count(?MODULE, Con, What).

get(Con, What) ->
    empdb_dao_doc:get(?MODULE, Con, What).

get(Con, What, Fields)->
    empdb_dao_doc:get(?MODULE, Con, What, Fields).

create(Con, Proplist)->
    empdb_dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    empdb_dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    empdb_dao_doc:is_owner(Con, Owner_id, Obj_id).


