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


empdb_dao_pers({fields, select})->
    [
        nick,
        email,
        phone,
        fname,
        sname,
        empl,
        hobby,
        descr,
        pregion_id,
        birthday,
        lang_id,
        lang_alias,
        ismale,
        money,
        pstatus_id,
        pstatus_alias,
        authority_id,
        authority_alias,
        exper,
        emotion_id,
        emotion_alias,
        mstatus_id,
        mstatus_alias,
        married_id,
        mother_id,
        father_id,
        community_id,
        community_head,
        live_room_id,
        live_room_head,
        isdeleted
    ].

count(Con, Proplist) ->
    empdb_dao:count(
        ?MODULE,
        Con,
        Proplist
    ).


get(Con, Proplist) ->
    Empdb_dao_pers = [
        {{table, name},             empdb_dao_pers:table(name)},
        {{table, fields, all},      empdb_dao_pers:table({fields, all})},
        {{table, fields, select},   empdb_dao_pers({fields, select})}
    ],
    
    empdb_dao:get(
        [{Empdb_dao_pers, id},
        {?MODULE, friend_id}],
        Con,
        Proplist
    ).

get(Con, Proplist, Fields) ->
    Empdb_dao_pers = [
        {{table, name},             empdb_dao_pers:table(name)},
        {{table, fields, all},      empdb_dao_pers:table({fields, all})},
        {{table, fields, select},   empdb_dao_pers({fields, select})}
    ],

    empdb_dao:get(
        [
            {Empdb_dao_pers, id},
            {?MODULE, friend_id}
        ],
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
