%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_doc).
-behaviour(dao).

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

-export([
    create/3,
    update/3,
    get/4
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
        title,
        content,
        doc_type_id,
        content_type_id,
        owner_id,
        parent_id
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
    doc.

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

is_owner(Con, {owner_id, Owner_id}, {id, Id})->
    case get(Con, {id, Id}, [owner_id]) of
        {ok, [{[{owner_id, Owner_id}]}]} ->
            true;
        _ ->
            false
    end.

is_owner(Con, Owner_id, Id)->
    is_owner(Con, {owner_id, Owner_id}, {id, Id}).


%%
%% @doc Возвращает экземпляр документа и экземпляр join-наследника.
%%      Наследник должен быть описан в модуле Module.
%%
get(Module, Con, What, Fields)->
    dao:get({?MODULE, id},    {Module, doc_id}, Con, What, Fields).

%%
%% @doc Создает экземпляр документа и экземпляр join-наследника.
%%      Наследник должен быть описан в модуле Module.
%%
create(Module, Con, Proplist)->
    dao:create({?MODULE, id}, {Module, doc_id}, Con, Proplist, doc_id).

%%
%% @doc Изменяет экземпляр документа и экземпляр join-наследника.
%%      Наследник должен быть описан в модуле Module.
%%
update(Module, Con, Proplist)->
    dao:update({?MODULE, id}, {Module, doc_id}, Con, Proplist).





