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



-export([
    get_acctype/2,
    get_acctype/3,
    create_acctype/2,
    update_acctype/2,
    get_doctype/2,
    get_doctype/3,
    create_doctype/2,
    update_doctype/2,
    get_contype/2,
    get_contype/3,
    create_contype/2,
    update_contype/2
]).



-export([
    get_oktype/2,
    get_oktype/3,
    create_oktype/2,
    update_oktype/2
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
    table({fields, all}) -- [id, isdeleted];

%%
%% @doc Возвращает список полей таблицы для создания
%%
table({fields, insert})->
    table({fields, all}) -- [id, isdeleted];

%%
%% @doc Возвращает полный список полей таблицы
%%
table({fields, all})->
    [
        id,
        head,
        body,
        oktype_id,
        doctype_id,
        contype_id,
        oktype_id,
        owner_id,
        parent_id,
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
    end;

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
    dao:create({?MODULE, id}, {Module, doc_id}, Con, Proplist).

%%
%% @doc Изменяет экземпляр документа и экземпляр join-наследника.
%%      Наследник должен быть описан в модуле Module.
%%
update(Module, Con, Proplist)->
    dao:update({?MODULE, id}, {Module, doc_id}, Con, Proplist).



get_acctype(Con, What) ->
    get_acctype(Con, What, []).

get_acctype(Con, What, Fields)->
    dao:get(acctype(), Con, What, Fields).

create_acctype(Con, Proplist)->
    dao:get(acctype(), Con, Proplist).

update_acctype(Con, Proplist)->
    dao:get(acctype(), Con, Proplist).


get_doctype(Con, What) ->
    get_doctype(Con, What, []).

get_doctype(Con, What, Fields)->
    dao:get(doctype(), Con, What, Fields).

create_doctype(Con, Proplist)->
    dao:get(doctype(), Con, Proplist).

update_doctype(Con, Proplist)->
    dao:get(doctype(), Con, Proplist).


get_contype(Con, What) ->
    get_contype(Con, What, []).

get_contype(Con, What, Fields)->
    dao:get(contype(), Con, What, Fields).

create_contype(Con, Proplist)->
    dao:get(contype(), Con, Proplist).

update_contype(Con, Proplist)->
    dao:get(contype(), Con, Proplist).


get_oktype(Con, What) ->
    get_oktype(Con, What, []).

get_oktype(Con, What, Fields)->
    dao:get(oktype(), Con, What, Fields).

create_oktype(Con, Proplist)->
    dao:create(oktype(), Con, Proplist).

update_oktype(Con, Proplist)->
    dao:update(oktype(), Con, Proplist).


oktype() ->
    [
        %% Имя таблицы.
        {{table, name},                       oktype},
        %% Список всех полей.
        {{table, fields, all},                [id, alias, name_ti, isdeleted]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [id, name_ti, alias]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [name_ti, alias]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [name_ti, alias]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].

acctype() ->
    [
        %% Имя таблицы.
        {{table, name},                       acctype},
        %% Список всех полей.
        {{table, fields, all},                [id, alias, name_ti, isdeleted]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [id, name_ti, alias]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [name_ti, alias]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [name_ti, alias]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].

doctype() ->
    [
        %% Имя таблицы.
        {{table, name},                       doctype},
        %% Список всех полей.
        {{table, fields, all},                [id, alias, name_ti, isdeleted]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [name_ti, id, alias]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [name_ti, alias]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [name_ti, alias]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].

contype() ->
    [
        %% Имя таблицы.
        {{table, name},                       contype},
        %% Список всех полей.
        {{table, fields, all},                [id, alias, name_ti, isdeleted]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [id, name_ti, alias]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [name_ti, alias]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [name_ti, alias]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].


