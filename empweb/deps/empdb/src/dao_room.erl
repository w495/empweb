%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_room).
-behaviour(dao).

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
    get/3
]).



%%
%% Exported Functions
%%
-export([
    update_chatlang/2,
    create_chatlang/2,
    get_chatlang/2,
    get_chatlang/3
]).




%%
%% Exported Functions
%%
-export([
    update_roomtype/2,
    create_roomtype/2,
    get_roomtype/2,
    get_roomtype/3
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
        type_id,
        ulimit,
        chatlang_id,
        topic_id,
        slogan,
        weather,
        treasury
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
    room.

table()->
    table(name).

get(Con, Some) ->
    get(Con, Some, []).

get(Con, What, Fields)->
    dao_doc:get(?MODULE, Con, What, Fields).

create(Con, Proplist)->
    dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    dao_doc:is_owner(Con, Owner_id, Obj_id).



get_roomtype(Con, What) ->
    get_roomtype(Con, What, []).

get_roomtype(Con, What, Fields)->
    dao:get(roomtype(), Con, What, Fields).

create_roomtype(Con, Proplist)->
    dao:get(roomtype(), Con, Proplist).

update_roomtype(Con, Proplist)->
    dao:get(roomtype(), Con, Proplist).


get_chatlang(Con, What) ->
    get_chatlang(Con, What, []).

get_chatlang(Con, What, Fields)->
    dao:get(chatlang(), Con, What, Fields).

create_chatlang(Con, Proplist)->
    dao:get(chatlang(), Con, Proplist).

update_chatlang(Con, Proplist)->
    dao:get(chatlang(), Con, Proplist).



%%
%% Local Functions
%%


roomtype() ->
    [
        {{table, name},                       roomtype},
        {{table, fields, all},                [id, name_ti, alias, isdeleted]},
        {{table, fields, select},             [id, name_ti, alias]},
        {{table, fields, insert},             [name_ti, alias]},
        {{table, fields, update},             [name_ti, alias]},
        {{table, fields, insert, required},   [alias]}
    ].


chatlang() ->
    [
        {{table, name},                       chatlang},
        {{table, fields, all},                [id, name_ti, alias, isdeleted]},
        {{table, fields, select},             [id, name_ti, alias]},
        {{table, fields, insert},             [name_ti, alias]},
        {{table, fields, update},             [name_ti, alias]},
        {{table, fields, insert, required},   [alias]}
    ].
