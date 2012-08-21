%% Author: w-495
%% Created: 25.07.2012
%% Description:
%% 
%%      Работа с таблицей пользователей и различными вспомогательными таблицами
%%
%%

-module(dao_pers).
-behaviour(dao).

%%
%% Include files
%%


%%
%% Exported Functions
%%
-export([
    table/0,
    table/1,
    create/2,
    update/2,
    get/2,
    get/3,
    get_perm/2,
    get_perm/3,
    get_group/2,
    get_group/3,
    get_friends/2,
    add_friend/2,
    delete_friend/2
]).

%%
%% Exported Functions
%%
-export([
    get_pgroup/2,
    get_pgroup/3,
    create_pgroup/2,
    update_pgroup/2
]).

%%
%% Exported Functions
%%
-export([
    get_pstatus/2,
    get_pstatus/3,
    create_pstatus/2,
    update_pstatus/2
]).

%%
%% Exported Functions
%%
-export([
    get_mstatus/2,
    get_mstatus/3,
    create_mstatus/2,
    update_mstatus/2
]).

%%
%% Exported Functions
%%
-export([
    get_authority/2,
    get_authority/3,
    create_authority/2,
    update_authority/2
]).


-export([
    get_emotion/2,
    get_emotion/3,
    create_emotion/2,
    update_emotion/2
]).



%%
%% API Functions
%%

%% 
%% @doc Возвращает список обязательных полей таблицы для создания
%%
table({fields, insert, required})->
    [nick, phash];

%%
%% @doc Возвращает список полей таблицы для выборки
%%
table({fields, select})->
    [   'extract(epoch from birthday) as birthday'
        |table({fields, all})
    ] -- [birthday];

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
        ismale,

        %% Информация о персоонаже
        money,
        pstatus_id,
        authority_id,
        emotion_id,

        mstatus_id,
        married_id,
        mother_id,
        father_id,
        community_id,
        room_id,
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


get_perm(Con, Params) ->
    get_perm(Con, Params, []).

get_perm(Con, Params, []) ->
    get_perm(Con, Params, [alias]);

get_perm(Con, Kvalue, Fields) when erlang:is_list(Kvalue) ->
    case {proplists:get_value(id, Kvalue), proplists:get_value(login, Kvalue)} of
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


get_group(Con, Params) ->
    get_group(Con, Params, []).

get_group(Con, Params, []) ->
    get_group(Con, Params, [alias]);

get_group(Con, Kvalue, Fields) when erlang:is_list(Kvalue) ->
    case {proplists:get_value(id, Kvalue), proplists:get_value(login, Kvalue)} of
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

%
% 230
%
add_friend(Con, Proplist)->
    case dao:pgret(
        dao:equery(Con,
            <<"insert into friend (pers_id, friend_id) "
            "values ($pers_id, $friend_id) returning id; ">>,
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
        ok ->
            {error, not_exists};
        Res ->
            Res
    end.

get_friends(Con, {id, User_id})->
    get_friends(Con, {pers_id, User_id});

get_friends(Con, {pers_id, User_id})->
    dao:pgret(
        dao:equery(Con, 
            <<"select friend.friend_id from friend "
            "where friend.pers_id = $pers_id">>,
            [User_id]
        )
    );

get_friends(Con, Proplist)->
    dao:pgret(
        dao:equery(Con,
            <<"select friend.friend_id from friend "
            "where friend.pers_id = $pers_id">>,
            Proplist
        )
    ).


get_pgroup(Con, What) ->
    get_pgroup(Con, What, []).

get_pgroup(Con, What, Fields)->
    dao:get(pgroup(), Con, What, Fields).

create_pgroup(Con, Proplist)->
    dao:create(pgroup(), Con, Proplist).

update_pgroup(Con, Proplist)->
    dao:update(pgroup(), Con, Proplist).




get_pstatus(Con, What) ->
    get_pstatus(Con, What, []).

get_pstatus(Con, What, Fields)->
    dao:get(pstatus(), Con, What, Fields).

create_pstatus(Con, Proplist)->
    dao:create(pstatus(), Con, Proplist).

update_pstatus(Con, Proplist)->
    dao:update(pstatus(), Con, Proplist).



get_mstatus(Con, What) ->
    get_mstatus(Con, What, []).

get_mstatus(Con, What, Fields)->
    dao:get(mstatus(), Con, What, Fields).

create_mstatus(Con, Proplist)->
    dao:create(mstatus(), Con, Proplist).

update_mstatus(Con, Proplist)->
    dao:update(mstatus(), Con, Proplist).




get_authority(Con, What) ->
    get_authority(Con, What, []).

get_authority(Con, What, Fields)->
    dao:get(authority(), Con, What, Fields).

create_authority(Con, Proplist)->
    dao:create(authority(), Con, Proplist).

update_authority(Con, Proplist)->
    dao:update(authority(), Con, Proplist).




get_emotion(Con, What) ->
    get_emotion(Con, What, []).

get_emotion(Con, What, Fields)->
    dao:get(emotion(), Con, What, Fields).

create_emotion(Con, Proplist)->
    dao:create(emotion(), Con, Proplist).

update_emotion(Con, Proplist)->
    dao:update(emotion(), Con, Proplist).


%%
%% Local Functions
%%
    
pgroup() ->
    [
        {{table, name},                       pgroup},
        {{table, fields, all},                [id, alias, name_ti, issystem, isdeleted]},
        {{table, fields, select},             [id, alias, name_ti]},
        {{table, fields, insert},             [alias, name_ti]},
        {{table, fields, update},             [id, alias, name_ti]},
        {{table, fields, insert, required},   [alias]}
    ].

pstatus() ->
    [
        {{table, name},                       pstatus},
        {{table, fields, all},                [id, alias, name_ti, isdeleted]},
        {{table, fields, select},             [id, alias, name_ti]},
        {{table, fields, insert},             [alias, name_ti]},
        {{table, fields, update},             [id, alias, name_ti]},
        {{table, fields, insert, required},   [alias]}
    ].

mstatus() ->
    [
        {{table, name},                       mstatus},
        {{table, fields, all},                [id, alias, name_ti, isdeleted]},
        {{table, fields, select},             [id, alias, name_ti]},
        {{table, fields, insert},             [alias, name_ti]},
        {{table, fields, update},             [id, alias, name_ti]},
        {{table, fields, insert, required},   [alias]}
    ].

authority() ->
    [
        {{table, name},                       authority},
        {{table, fields, all},                [id, alias, name_ti, level, isdeleted]},
        {{table, fields, select},             [id, alias, level, name_ti]},
        {{table, fields, insert},             [alias, level, name_ti]},
        {{table, fields, update},             [id, alias, level, name_ti]},
        {{table, fields, insert, required},   [alias, level]}
    ].

emotion() ->
    [
        {{table, name},                       emotion},
        {{table, fields, all},                [id, alias, name_ti, isdeleted]},
        {{table, fields, select},             [id, alias, name_ti]},
        {{table, fields, insert},             [alias, name_ti]},
        {{table, fields, update},             [id, alias, name_ti]},
        {{table, fields, insert, required},   [alias]}
    ].

