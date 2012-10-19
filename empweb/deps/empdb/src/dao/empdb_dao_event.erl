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
    update_eventtype/2,
    create_eventtype/2,
    get_eventtype/2,
    get_eventtype/3
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
        oktype_id,
        slogan,
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
    event.

table()->
    table(name).

get(Con, Some) ->
    get(Con, Some, []).

get(Con, What, Fields)->
    empdb_dao_doc:get(?MODULE, Con, What, Fields).

create(Con, Proplist)->
    empdb_dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    empdb_dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    empdb_dao_doc:is_owner(Con, Owner_id, Obj_id).



get_eventtype(Con, What) ->
    get_eventtype(Con, What, []).

get_eventtype(Con, What, Fields)->
    empdb_dao:get(eventtype(), Con, What, Fields).

create_eventtype(Con, Proplist)->
    empdb_dao:get(eventtype(), Con, Proplist).

update_eventtype(Con, Proplist)->
    empdb_dao:get(eventtype(), Con, Proplist).

%%
%% Local Functions
%%

%%
%% @doc Описывает типы сообщений
%%
eventtype() ->
    [
        %% Имя таблицы.
        {{table, name},                       eventtype},
        %% Список всех полей.
        {{table, fields, all},                [
            id, name_ti, alias, isdeleted
        ]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [
            id, name_ti, alias
        ]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [
            name_ti, alias
        ]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [
            name_ti, alias
        ]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [
            alias
        ]}
    ].



