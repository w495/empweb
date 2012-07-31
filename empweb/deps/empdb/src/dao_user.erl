%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_user).

%%
%% Include files
%%

-include("user.hrl").

%%
%% Exported Functions
%%
-export([
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


fields()->
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
    ].


selectables() ->
    fields().


filter_fields(List) ->
    lists:filter(fun is_field/1, List).

fields(Pl) ->
    filter_fields(proplists:get_keys(Pl)).

is_field(Mbfield)->
    is_field(Mbfield, fields()).

is_field(Mbfield, Fields)->
    lists:member(Mbfield, Fields).


get(Con, {id, Id})->
    get(Con, {id, Id}, []);

get(Con, {name, Name})->
    get(Con, {name, Name}, []);

get(Con, {nick, Name})->
    get(Con, {nick, Name}, []);

get(Con, Some) ->
    get(Con, Some, []).

get(Con, {id, Id}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, selectables()),
                <<" from user_ where id = $1">>
            ],
            [Id]
        )
    );

get(Con, {name, Name}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, selectables()),
                <<" from user_ where name = $1">>
            ],
            [Name]
        )
    );


get(Con, {nick, Nick}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, selectables()),
                <<" from user_ where nick = $1">>
            ],
            [Nick]
        )
    );

get(Con, _, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, selectables()),
                <<" from user_">>
            ]
        )
    ).

create(Con, Proplist)->
    io:format("Proplist = ~p~n", [Proplist]),
    Fields = fields(Proplist),
    dao:pgret(
        dao:equery(Con,[
            <<"insert into user_ (">>,
                dao:fields(Fields),
            <<") values (">>,
                dao:fieldvars(Fields),
            <<") returning id; ">>
        ],Proplist)
    ).

update(Con, Proplist)->
    Fields = fields(Proplist),
    case proplists:get_value(id, Proplist) of
        undefined -> 
            create(Con, Proplist);
        Id ->
            dao:pgret(
                dao:equery(Con,[
                    <<"update  user_ set">>,
                        dao:fields_fieldvars(Fields),
                    <<"where id= $id">>
                ],Proplist)
            ),
            {ok, Id}
    end.


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
    dao:pgret(
        dao:equery(Con, 
            <<"insert into friend (user_id, friend_id) "
            "values ($user_id, $friend_id) returning id; ">>, 
            Proplist
        )
    ).

delete_friend(Con, Proplist)->
    dao:pgret(
        dao:equery(Con, 
            <<"delete from friend where "
            " user_id=$user_id and friend_id=$friend_id">>, 
            Proplist
        )
    ).

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

