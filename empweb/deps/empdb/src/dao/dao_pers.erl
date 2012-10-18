%%
%%      Работа с таблицей пользователей и различными вспомогательными таблицами
%%
%%

-module(dao_pers).
-behaviour(dao).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%% ===========================================================================
%% Экспортируемые функции
%% ===========================================================================

%%
%% Группа пользователя
%%
-export([
    table/0,
    table/1,
    create/2,
    update/2,
    update/3,
    get/2,
    get/3,
    count/2,
    get_perm/2,
    get_perm/3,
    get_group/2,
    get_group/3
]).

%%
%% Группа пользователя
%%
-export([
    get_pgroup/2,
    get_pgroup/3,
    create_pgroup/2,
    update_pgroup/2
]).

%%
%% Группа пользователя
%%
-export([
    get_friends/2,
    add_friend/2,
    delete_friend/2
]).

%%
%% Статус пользователя
%%
-export([
    get_pstatus/2,
    get_pstatus/3,
    create_pstatus/2,
    update_pstatus/2
]).

%%
%% Семейное положение пользователя
%%
-export([
    get_mstatus/2,
    get_mstatus/3,
    create_mstatus/2,
    update_mstatus/2
]).

%%
%% Авторитет пользователя
%%
-export([
    get_authority/2,
    get_authority/3,
    create_authority/2,
    update_authority/2
]).

%%
%% Эмоции пользователя
%%
-export([
    get_emotion/2,
    get_emotion/3,
    create_emotion/2,
    update_emotion/2
]).

%%
%% Ejabberd-aккаунт пользователя
%%
-export([
    get_ejabberd/2,
    get_ejabberd/3,
    create_ejabberd/2,
    update_ejabberd/2
]).

%% ===========================================================================
%% Внешние функции
%% ===========================================================================

%% 
%% @doc Возвращает список обязательных полей таблицы для создания
%%
table({fields, insert, required})->
    [phash];

%%
%% @doc Возвращает список полей таблицы для выборки
%%
table({fields, select})->
    [   'extract(epoch from birthday) as birthday'
        |table({fields, all})
    ] -- [birthday, isdeleted];

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
        %% Идентификация
        id,
        login, 
        nick,
        phash,
        email,
        phone,

        %% Информация о пользователе
        fname,
        sname,
        empl,
        hobby,
        descr,
        pregion_id,
        birthday,    % 'extract(epoch from birthday) as birthday',
        lang_id,
        lang_alias,
        ismale,

        %% Информация о персоонаже
        money,
        pstatus_id,
        pstatus_alias,
        authority_id,
        authority_alias,
        
        exper,
        experlack,
        
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

        own_room_id,
        own_room_head,
        
        allowauctionoffer,
        perspicbody_id,
        perspichead_id,
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
    pers.


table()->
    table(name).


get(Con, What) ->
    dao:get(?MODULE, Con, What).

count(Con, What) ->
    dao:count(?MODULE, Con, What).

get(Con, What, Fields)->
    dao:get(?MODULE, Con, What, Fields).

create(Con, Proplist)->
    dao:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    dao:update(?MODULE, Con, Proplist).

update(Con, Proplist, Where)->
    dao:update(?MODULE, Con, Proplist, Where).
    
is_owner(Con, Id, Id)->
    true;
is_owner(Con, _, _)->
    false.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Системные права пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_perm(Con, Params) ->
    get_perm(Con, Params, []).

get_perm(Con, Params, []) ->
    get_perm(Con, Params, [alias]);

get_perm(Con, Kvalue, Fields) when erlang:is_list(Kvalue) ->
    case {
        proplists:get_value(id, Kvalue),
        proplists:get_value(login, Kvalue)
    } of
        {undefined, undefined   } ->
            {error, {no_data,no_data}};
        {Id,        undefined   } ->
            get_perm(Con, {id, Id}, Fields);
        {undefined, Login        } ->
            get_perm(Con, {login, Login}, Fields);
        {Id,        Login        } ->
            get_perm(Con, {id, Id}, Fields)
    end;

get_perm(Con, {id, Id}, Fields) ->
    dao:pgret(
        dao:equery(Con,[
            <<"select distinct ">>,
                dao:table_fields(perm, Fields),
            <<" from perm "
                "join perm2pgroup on "
                    " perm2pgroup.perm_id = perm.id "
                "join pers2pgroup on "
                    " pers2pgroup.group_id = perm2pgroup.group_id"
                    " and pers2pgroup.pers_id = $1">>
            ],[Id]
        )
    );

get_perm(Con, {login, Login}, Fields) ->
    dao:pgret(
        dao:equery(Con,[
            <<"select distinct ">>,
                dao:table_fields(perm, Fields),
            <<" from perm "
                "join perm2pgroup on "
                    " perm2pgroup.perm_id = perm.id "
                "join pers2pgroup on "
                    " pers2pgroup.group_id = perm2pgroup.group_id "
                "join pers on "
                    "pers2pgroup.pers_id = pers.id "
                    "and pers.login = $1">>
            ],[Login]
        )
    ).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Группы пользователя (доступ конкретно по id пользователя)
%% 
%% WARNING: По написаниею оно похоже на на опирации с группами непосредственно.
%%          Возможно, надо перекидать сущности по модулям.
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_group(Con, Params) ->
    get_group(Con, Params, []).

get_group(Con, Params, []) ->
    get_group(Con, Params, [alias]);

get_group(Con, Kvalue, Fields) when erlang:is_list(Kvalue) ->
    case {
        proplists:get_value(id, Kvalue),
        proplists:get_value(login, Kvalue)
    } of
        {undefined, undefined   } ->
            {error, {no_data,no_data}};
        {Id,        undefined   } ->
            get_group(Con, {id, Id}, Fields);
        {undefined, Login        } ->
            get_group(Con, {login, Login}, Fields);
        {Id,        Login        } ->
            get_group(Con, {id, Id}, Fields)
    end;

get_group(Con, {id, Id}, Fields) ->
    dao:pgret(
        dao:equery(Con,[
            <<"select distinct ">>,
                dao:table_fields(pers_group, Fields),
            <<" from pgroup "
                "join pers2pgroup on "
                    " pers2pgroup.group_id = pers_group.id "
                    " and pers2pgroup.pers_id = $1">>
            ],[Id]
        )
    );

get_group(Con, {login, Login}, Fields) ->
    dao:pgret(
        dao:equery(Con,[
            <<"select distinct ">>,
                dao:table_fields(pers_group, Fields),
            <<" from pgroup "
                "join pers2pgroup on "
                    " pers2pgroup.group_id = pers_group.id "
                "join pers on "
                    "pers2pgroup.pers_id = pers.id "
                    "and pers.login = $1">>
            ],[Login]
        )
    ).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Группы пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_pgroup(Con, What) ->
    get_pgroup(Con, What, []).

get_pgroup(Con, What, Fields)->
    dao:get(pgroup(), Con, What, Fields).

create_pgroup(Con, Proplist)->
    dao:create(pgroup(), Con, Proplist).

update_pgroup(Con, Proplist)->
    dao:update(pgroup(), Con, Proplist).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Друзья пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_friend(Con, Proplist)->
    case dao:pgret(
        dao:equery(Con,
            <<"insert into friend (pers_id, friend_id) "
                "values ($pers_id, $friend_id) "
                "returning id">>,
            Proplist
        )
    ) of
        {error,{not_unique,<<"pers_id_friend_id_many">>}} ->
            {error, {not_unique, [pers_id, friend_id]}};
        Res ->
            Res
    end.

delete_friend(Con, Proplist)->
    case dao:pgret(
        dao:equery(Con,
            <<"delete from friend where "
            " pers_id=$pers_id and friend_id=$friend_id returning id">>,
            Proplist
        )
    ) of
        {ok, 0} ->
            {error, not_exists};
        Res ->
            Res
    end.

get_friends(Con, {id, User_id})->
    get_friends(Con, [{pers_id, User_id}]);

get_friends(Con, {pers_id, User_id})->
    get_friends(Con, [{pers_id, User_id}]);

get_friends(Con, Proplist)->
    dao:eqret(Con,
        <<"select "
            " pers.id as id, "
            " pstatus.alias as pstatus_alias, "
            " pers.nick as nick, "
            " doc.head as live_room_head, "
            " doc.id as live_room_id "
        " from "
            " friend "
        " join pers on "
            " friend.friend_id = pers.id "
        " left join pstatus on "
            " pstatus.id = pers.pstatus_id "
        " left join doc on "
            " doc.id = pers.live_room_id "
        " join room on "
            " room.doc_id = doc.id "
        " where "
            " friend.pers_id = $pers_id; ">>,
        Proplist
    ).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Статус пользователя пользователя: в сети \ не в сети.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_pstatus(Con, What) ->
    get_pstatus(Con, What, []).

get_pstatus(Con, What, Fields)->
    dao:get(pstatus(), Con, What, Fields).

create_pstatus(Con, Proplist)->
    dao:create(pstatus(), Con, Proplist).

update_pstatus(Con, Proplist)->
    dao:update(pstatus(), Con, Proplist).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Семейное положение пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_mstatus(Con, What) ->
    get_mstatus(Con, What, []).

get_mstatus(Con, What, Fields)->
    dao:get(mstatus(), Con, What, Fields).

create_mstatus(Con, Proplist)->
    dao:create(mstatus(), Con, Proplist).

update_mstatus(Con, Proplist)->
    dao:update(mstatus(), Con, Proplist).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Авторитет пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_authority(Con, What) ->
    get_authority(Con, What, []).

get_authority(Con, What, Fields)->
    dao:get(authority(), Con, What, Fields).

create_authority(Con, Proplist)->
    dao:create(authority(), Con, Proplist).

update_authority(Con, Proplist)->
    dao:update(authority(), Con, Proplist).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Эмоции пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_emotion(Con, What) ->
    get_emotion(Con, What, []).

get_emotion(Con, What, Fields)->
    dao:get(emotion(), Con, What, Fields).

create_emotion(Con, Proplist)->
    dao:create(emotion(), Con, Proplist).

update_emotion(Con, Proplist)->
    dao:update(emotion(), Con, Proplist).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejabberd-aккаунт пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_ejabberd(Con, What) ->
    get_ejabberd(Con, What, []).

get_ejabberd(Con, What, Fields)->
    dao:get(ejabberd(), Con, What, Fields).

create_ejabberd(Con, Proplist)->
    dao:create(ejabberd(), Con, Proplist, [username]).

update_ejabberd(Con, Proplist)->
    dao:update(ejabberd(), Con, Proplist, [username]).

%% ===========================================================================
%% Внутренние функции
%% ===========================================================================

%% 
%% @doc Описывает группу пользователя
%%
pgroup() ->
    [
        %% Имя таблицы.
        {{table, name},                       pgroup},
        %% Список всех полей.
        {{table, fields, all},                [
            id, alias, name_ti, issystem, isdeleted
        ]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [
            id, alias, name_ti
        ]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [
            alias, name_ti
        ]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [
            id, alias, name_ti
        ]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [
            alias
        ]}
    ].

%%
%% @doc Описывает статус пользователя
%%
pstatus() ->
    [
        %% Имя таблицы.
        {{table, name},                       pstatus},
        %% Список всех полей.
        {{table, fields, all},                [
            id, alias, name_ti, isdeleted
        ]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [
            id, alias, name_ti
        ]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [
            alias, name_ti
        ]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [
            id, alias, name_ti
        ]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [
            alias
        ]}
    ].

%%
%% @doc Описывает семейное положение пользователя
%%
mstatus() ->
    [
        %% Имя таблицы.
        {{table, name},                       mstatus},
        %% Список всех полей.
        {{table, fields, all},                [
            id, alias, name_ti, isdeleted
        ]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [
            id, alias, name_ti
        ]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [
            alias, name_ti
        ]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [
            id, alias, name_ti
        ]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].

%%
%% @doc Описывает семейное положение пользователя
%%
authority() ->
    [
        %% Имя таблицы.
        {{table, name},                       authority},
        %% Список всех полей.
        {{table, fields, all},                [
            id, alias, name_ti, level, isdeleted
        ]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [
            id, alias, level, name_ti
        ]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [
            alias, level, name_ti
        ]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [
            id, alias, level, name_ti
        ]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [
            alias, level
        ]}
    ].

%%
%% @doc Описывает эмоции пользователя
%%
emotion() ->
    [
        %% Имя таблицы.
        {{table, name},                       emotion},
        %% Список всех полей.
        {{table, fields, all},                [
            id, alias, name_ti, isdeleted
        ]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [
            id, alias, name_ti
        ]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [
            alias, name_ti
        ]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [
            id, alias, name_ti
        ]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [
            alias
        ]}
    ].

%%
%% @doc Описывает ejabberd-aккаунт пользователя
%%
ejabberd() ->
    [
        %% Имя таблицы.
        {{table, name},                       users},
        %% Список всех полей.
        {{table, fields, all},                [
            username, password
        ]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [
            username, password
        ]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [
            username, password
        ]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [
            username, password
        ]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [
            username, password
        ]}
    ].

