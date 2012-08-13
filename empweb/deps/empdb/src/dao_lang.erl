%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_lang).
-behaviour(dao).

%%
%% Include files
%%


%%
%% Exported Functions
%%
-export([
    create/2,
    update/2,
    get/2,
    get/3
]).

%%
%% API Functions
%%


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
        alias,
        name,
        description
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
    lang.

table()->
    table(name).


get(Con, Some) ->
    get(Con, Some, []).

get(Con, What, Fields)->
    dao:get(?MODULE, Con, What, Fields).

create(Con, Proplist)->
    dao:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    dao:update(?MODULE, Con, Proplist).

is_owner(Con, Id, Id)->
    true;
is_owner(Con, _, _)->
    false.


%%
%% Local Functions
%%

