%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_message).
-behaviour(dao).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%% ===========================================================================
%% Экспортируемые функции
%% ===========================================================================

%%
%% Само сообщение непосредственно
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
%% Типы сообщений
%%
-export([
    update_messagetype/2,
    create_messagetype/2,
    get_messagetype/2,
    get_messagetype/3
]).

%% ===========================================================================
%% Внешние функции
%% ===========================================================================

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Сами сообщения
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    [isdeleted|table({fields, all})] -- [id];

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
        reader_id
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
    message.

table()->
    table(name).

get(Con, What) ->
    dao_doc:get(?MODULE, Con, What).

get(Con, What, Fields)->
    dao_doc:get(?MODULE, Con, What, Fields).

create(Con, Proplist)->
    dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    dao_doc:is_owner(Con, Owner_id, Obj_id).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы сообщений
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_messagetype(Con, What) ->
    dao:get(messagetype(), Con, What).

get_messagetype(Con, What, Fields)->
    dao:get(messagetype(), Con, What, Fields).

create_messagetype(Con, Proplist)->
    dao:get(messagetype(), Con, Proplist).

update_messagetype(Con, Proplist)->
    dao:get(messagetype(), Con, Proplist).

%% ===========================================================================
%% Внутренние функции
%% ===========================================================================

%%
%% @doc Описывает типы сообщений
%%
messagetype() ->
    [
        %% Имя таблицы.
        {{table, name},                       messagetype},
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

