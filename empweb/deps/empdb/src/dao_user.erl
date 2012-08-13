%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_user).
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
%% API Functions
%%

%% 
%% @doc Возвращает список обязательных полей таблицы для создания
%%
table({fields, insert, required})->
    [nick, phash, email];

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
        nick,
        name,
        phash,
        email,
        phone,
        fname,
        sname,
        'extract(epoch from birthday) as birthday',
        male    ,
        city    ,
        married_status,
        married_id    ,
        description   ,
        money         ,
        status_id     ,
        authority_id  ,
        country_id    ,
        emotion_id    ,
        mother_id     ,
        father_id     ,
        community_id  ,
        employment    ,
        hobby         ,
        allow_auction_offer
        %,
        %userpic_body_id    ,
        %userpic_head_id
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
    user_.


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
    get_perm(Con, Params, [name]);

get_perm(Con, {id, Id}, Fields) ->
    dao:pgret(
        dao:equery(Con,[
            <<"select distinct ">>,
                dao:table_fields(perm, Fields),
            <<" from perm "
                "join perm2group on "
                    " perm2group.perm_id = perm.id "
                "join user2group on "
                    " user2group.group_id = perm2group.group_id"
                    " and user2group.user_id = $1">>
            ],[Id]
        )
    );

get_perm(Con, {nick, Nick}, Fields) ->
    dao:pgret(
        dao:equery(Con,[
            <<"select distinct ">>,
                dao:table_fields(perm, Fields),
            <<" from perm "
                "join perm2group on "
                    " perm2group.perm_id = perm.id "
                "join user2group on "
                    " user2group.group_id = perm2group.group_id "
                "join user_ on "
                    "user2group.user_id = user_.id "
                    "and user_.nick = $1">>
            ],[Nick]
        )
    ).

get_group(Con, {id, Id}) ->
    get_group(Con, {id, Id}, []).

get_group(Con, {id, Id}, []) ->
    get_group(Con, {id, Id}, [name]);

get_group(Con, {id, Id}, Fields) ->
    dao:pgret(
        dao:equery(Con,[
            <<"select distinct ">>,
                dao:table_fields(user_group, Fields),
            <<" from user_group "
                "join user2group on "
                    " user2group.group_id = user_group.id "
                    " and user2group.user_id = $1">>
            ],[Id]
        )
    ).


%
% 230
%
add_friend(Con, Proplist)->
    case dao:pgret(
        dao:equery(Con,
            <<"insert into friend (user_id, friend_id) "
            "values ($user_id, $friend_id) returning id; ">>, 
            Proplist
        )
    ) of
        {error,{not_unique,<<"user_id_friend_id_many">>}} ->
            {error, {not_unique, [user_id, friend_id]}};
        Res ->
            Res
    end.

delete_friend(Con, Proplist)->
    case dao:pgret(
        dao:equery(Con, 
            <<"delete from friend where "
            " user_id=$user_id and friend_id=$friend_id returning id">>, 
            Proplist
        )
    ) of
        ok ->
            {error, not_exists};
        Res ->
            Res
    end.

get_friends(Con, {id, User_id})->
    get_friends(Con, {user_id, User_id});

get_friends(Con, {user_id, User_id})->
    dao:pgret(
        dao:equery(Con, 
            <<"select friend.friend_id from friend "
            "where friend.user_id = $user_id">>, 
            [User_id]
        )
    );

get_friends(Con, Proplist)->
    dao:pgret(
        dao:equery(Con,
            <<"select friend.friend_id from friend "
            "where friend.user_id = $user_id">>, 
            Proplist
        )
    ).

%%
%% Local Functions
%%


    
