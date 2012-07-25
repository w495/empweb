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
    get_friends/2,
    add_friend/2,
    delete_friend/2
]).

%%
%% API Functions
%%

get(Con, {id, Id})->
    dao:pgret(
        dao:equery(Con, 
            <<"select * from user_ where id = $1">>, 
            [Id]
        )
    );

get(Con, {name, Name})->
    dao:pgret(
        dao:equery(Con, 
            <<"select * from user_ where name = $1">>, 
            [Name]
        )
    );

get(Con, {nick, Nick})->
    dao:pgret(
        dao:equery(Con, 
            <<"select * from user_ where nick = $1">>, 
            [Nick]
        )
    );

get(Con, _)->
    dao:pgret(
        dao:equery(Con, 
            <<"select * from user_ where">>
        )
    ).


create(Con, Proplist)->
    io:format("Proplist = ~p~n", [Proplist]),
    dao:pgret(
        dao:equery(Con,
            <<"insert into user_ "
                "(nick,description,phash,email,phone,fname,sname,city)"
            "values "
                "($nick,$description,$phash,$email,$phone,$fname,$sname,$city) "
            " returning id; ">>, 
            Proplist
        )
    ).

update(Con, Proplist)->
    case proplists:get_value(id, Proplist) of
        undefined -> 
            create(Con, Proplist);
        Id ->
            dao:pgret(
                dao:equery(Con,
                    <<"update  user_ set"
                        "nick=$nick,"
                        "name=$nick,"
                        "description=$description,"
                        "phash=$phash,"
                        "email=$email,"
                        "phone=$phone,"
                        "fname=$fname,"
                        "sname=$sname,"
                        "city=$city,"
                    "where id=$id">>, 
                    Proplist
                )
            )
    end.

%
% 230
%
add_friend(Con, Proplist)->
    dao:pgret(
        dao:equery(Con, 
            <<"insert into friend (user_id, friend_id) "
            "values ($user_id, $friend_id)">>, 
            Proplist
        )
    ).

delete_friend(Con, Proplist)->
    dao:pgret(
        dao:equery(Con, 
            <<"delete from friend where "
            " user_id=$user_id and friend_id=$friend_id;">>, 
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

