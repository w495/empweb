%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_emotion).

%%
%% Include files
%%



%%
%% Exported Functions
%%
-export([
    create/2,
    update/2,
    get/2,
    get/3
]).

%%
%% API Functions
%%


fields()->
    [
        id,
        name_cid
    ].

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

get(Con, {name_cid, Name})->
    get(Con, {name_cid, Name}, []);

get(Con, Some) ->
    get(Con, Some, []).

get(Con, {id, Id}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields),
                <<" from emotion where id = $1">>
            ],
            [Id]
        )
    );

get(Con, {name_cid, Name}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields),
                <<" from emotion where name_cid = $1">>
            ],
            [Name]
        )
    );

get(Con, _, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields),
                <<" from emotion">>
            ]
        )
    ).

create(Con, Proplist)->
    io:format("Proplist = ~p~n", [Proplist]),
    Fields = fields(Proplist),
    dao:pgret(
        dao:equery(Con,[
            <<"insert into emotion (">>,
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
                    <<"update  emotion set">>,
                        dao:fields_fieldvars(Fields),
                    <<"where id= $id">>
                ],Proplist)
            ),
            {ok, Id}
    end.



%%
%% Local Functions
%%

