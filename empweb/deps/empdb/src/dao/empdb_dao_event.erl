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
        id,
        head,
        body,
        owner_id,
        owner_nick,
        pers_id,
        pers_nick,
        friendtype_id,
        friendtype_alias,
        eventtype_id,
        eventtype_alias,
        doc_id,
        doc_head,
        doc_owner_id,
        doc_owner_nick,
        doc_parent_id,
        doctype_id,
        doctype_alias,
        orig_id,
        orig_owner_id,
        orig_owner_nick,
        isnews,
        created,
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
    event.

table()->
    table(name).


count(Con, What) ->
    empdb_dao:count(?MODULE, Con, What).

get(Con, What) ->
    empdb_dao:get(?MODULE, Con, What).

get(Con, What, Fields)->
    empdb_dao:get(?MODULE, Con, What, Fields).

create(Con, Proplist)->
    empdb_dao:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    empdb_dao:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    empdb_dao:is_owner(Con, Owner_id, Obj_id).


