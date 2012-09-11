%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_room).
-behaviour(dao).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%% ===========================================================================
%% Экспортируемые функции
%% ===========================================================================

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
%% Типы комнат: страна, тюрьма, рай, ад.
%%
-export([
    update_roomtype/2,
    create_roomtype/2,
    get_roomtype/2,
    get_roomtype/3
]).


%%
%% Языки комнаты.
%%
-export([
    update_chatlang/2,
    create_chatlang/2,
    get_chatlang/2,
    get_chatlang/3
]).

%%
%% Режимы комнаты: дектатура, демократия и пр.
%%
-export([
    update_regimen/2,
    create_regimen/2,
    get_regimen/2,
    get_regimen/3
]).


%%
%% Темы чата комнаты.
%%
-export([
    update_topic/2,
    create_topic/2,
    get_topic/2,
    get_topic/3
]).


%% ===========================================================================
%% Внешние функции
%% ===========================================================================

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Сами комнаты
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
        regimen_id,
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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы комнат: страна, тюрьма, рай, ад.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_roomtype(Con, What) ->
    get_roomtype(Con, What, []).

get_roomtype(Con, What, Fields)->
    dao:get(roomtype(), Con, What, Fields).

create_roomtype(Con, Proplist)->
    dao:get(roomtype(), Con, Proplist).

update_roomtype(Con, Proplist)->
    dao:get(roomtype(), Con, Proplist).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Языки комнаты
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_chatlang(Con, What) ->
    get_chatlang(Con, What, []).

get_chatlang(Con, What, Fields)->
    dao:get(chatlang(), Con, What, Fields).

create_chatlang(Con, Proplist)->
    dao:get(chatlang(), Con, Proplist).

update_chatlang(Con, Proplist)->
    dao:get(chatlang(), Con, Proplist).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Режимы комнаты: дектатура, демократия и пр.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_regimen(Con, What) ->
    get_regimen(Con, What, []).

get_regimen(Con, What, Fields)->
    dao:get(regimen(), Con, What, Fields).

create_regimen(Con, Proplist)->
    dao:get(regimen(), Con, Proplist).

update_regimen(Con, Proplist)->
    dao:get(regimen(), Con, Proplist).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Темы чата комнаты.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_topic(Con, What) ->
    get_topic(Con, What, []).

get_topic(Con, What, Fields)->
    dao:get(topic(), Con, What, Fields).

create_topic(Con, Proplist)->
    dao:get(topic(), Con, Proplist).

update_topic(Con, Proplist)->
    dao:get(topic(), Con, Proplist).

%% ===========================================================================
%% Внутренние функции
%% ===========================================================================

%%
%% @doc Описывает типы комнат: страна, тюрьма, рай, ад.
%%
roomtype() ->
    [
        %% Имя таблицы.
        {{table, name},                       roomtype},
        %% Список всех полей.
        {{table, fields, all},                [id, name_ti, alias, isdeleted]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [id, name_ti, alias]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [name_ti, alias]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [name_ti, alias]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].

%%
%% @doc Описывает список языков чата, 
%% он не обязан пересекаться с таблицей lang.
%%
chatlang() ->
    [
        %% Имя таблицы.
        {{table, name},                       chatlang},
        %% Список всех полей.
        {{table, fields, all},                [id, name_ti, alias, isdeleted]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [id, name_ti, alias]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [name_ti, alias]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [name_ti, alias]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].

%%
%% @doc Описывает режимы комнаты: дектатура, демократия и пр.
%%
regimen() ->
    [
        %% Имя таблицы.
        {{table, name},                       regimen},
        %% Список всех полей.
        {{table, fields, all},                [id, name_ti, alias, isdeleted]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [id, name_ti, alias]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [name_ti, alias]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [name_ti, alias]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].

%%
%% @doc Описывает темы чата комнаты. 
%% Сами по себе темы представлют из себя дерево:
%% -------------------------------------------------
%%  [Все темы]
%%      |-[Автомобили]
%%          |-[Хорошие]
%%              |-[Чайка]
%%              |-[Уазик]
%%          |-[Плохие]
%%              |-[Калина]
%%              |-[Запорожец]
%%      ...
%% -------------------------------------------------
%% 
topic() ->
    [
        %% Имя таблицы.
        {{table, name},                       topic},
        %% Список всех полей.
        {{table, fields, all},                [
            id,
            alias,
            name_ti,    %% имя на нескольких языках.
            descr_ti,   %% описание на нескольких языках.
            parent_id,  %% ссылка на родительскую тему
            isdeleted
        ]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [
            id, name_ti, descr_ti, parent_id, alias
        ]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [
            name_ti, descr_ti, parent_id, alias
        ]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [
            name_ti, descr_ti, parent_id, alias
        ]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [
            name_ti, descr_ti, parent_id, alias
        ]}
    ].
