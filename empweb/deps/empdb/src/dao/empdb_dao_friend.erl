%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_dao_friend).
-behaviour(empdb_dao).

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
    count/2,
    get/2,
    get/3
]).

%%
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
        id          ,
        pers_id     ,
        friend_id   ,
        created     
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
    friend.

table()->
    table(name).


count(Con, Proplist) ->
    empdb_dao:count(
        ?MODULE,
        Con,
        Proplist
    ).


get(Con, Proplist) ->
    empdb_dao:get(
        [{empdb_dao_pers, id}, {?MODULE, friend_id}],
        Con,
        Proplist
    ).

get(Con, Proplist, Fields) ->
    empdb_dao:get(
         [{empdb_dao_pers, id}, {?MODULE, friend_id}],
        Con,
        Proplist,
        Fields
    ).

create(Con, Proplist) ->
    empdb_dao:create(
        ?MODULE,
        Con,
        Proplist
    ).

update(Con, Proplist) ->
    empdb_dao:update(
        ?MODULE,
        Con,
        Proplist
    ).
