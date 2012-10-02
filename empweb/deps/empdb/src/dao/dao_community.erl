%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_community).
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
        communitytype_id,
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



get_communitytype(Con, What) ->
    get_communitytype(Con, What, []).

get_communitytype(Con, What, Fields)->
    dao:get(communitytype(), Con, What, Fields).

create_communitytype(Con, Proplist)->
    dao:get(communitytype(), Con, Proplist).

update_communitytype(Con, Proplist)->
    dao:get(communitytype(), Con, Proplist).



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

