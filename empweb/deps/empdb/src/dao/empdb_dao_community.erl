%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_dao_community).
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
    count/2,
    get/2,
    get/3
]).



%%
%% Exported Functions
%%
-export([
    update_communitytype/2,
    create_communitytype/2,
    get_communitytype/2,
    get_communitytype/3
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
        nmembs,
        ncands,
        communitytype_id,
        communitytype_alias,
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
    community.

table()->
    table(name).

count(Con, Some) ->
    empdb_dao_doc:count(?MODULE, Con, Some).

get(Con, Some) ->
    empdb_dao_doc:get(?MODULE, Con, Some).

get(Con, What, Fields)->
    empdb_dao_doc:get(?MODULE, Con, What, Fields).

create(Con, Proplist)->
    empdb_dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    empdb_dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    empdb_dao_doc:is_owner(Con, Owner_id, Obj_id).



get_communitytype(Con, What) ->
    get_communitytype(Con, What, []).

get_communitytype(Con, What, Fields)->
    empdb_dao:get(communitytype(), Con, What, Fields).

create_communitytype(Con, Proplist)->
    empdb_dao:get(communitytype(), Con, Proplist).

update_communitytype(Con, Proplist)->
    empdb_dao:get(communitytype(), Con, Proplist).



%%
%% Local Functions
%%


communitytype() ->
    [
        {{table, name},                       communitytype},
        {{table, fields, all},                [id, name_ti, alias, isdeleted]},
        {{table, fields, select},             [id, name_ti, alias]},
        {{table, fields, insert},             [name_ti, alias]},
        {{table, fields, update},             [name_ti, alias]},
        {{table, fields, insert, required},   [alias]}
    ].

