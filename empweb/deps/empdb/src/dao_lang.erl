%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_lang).

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
        alias,
        name,
        description
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

get(Con, {name, Name})->
    get(Con, {name, Name}, []);

get(Con, {alias, Name})->
    get(Con, {alias, Name}, []);

get(Con, Some) ->
    get(Con, Some, []).

get(Con, {id, Id}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields),
                <<" from lang where id = $1">>
            ],
            [Id]
        )
    );

get(Con, {name, Name}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields),
                <<" from lang where name = $1">>
            ],
            [Name]
        )
    );


get(Con, {alias, Alias}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields),
                <<" from lang where alias = $1">>
            ],
            [Alias]
        )
    );

get(Con, {Key, Value}, Fields) when erlang:is_atom(Key) ->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields),
                <<" from lang where">>,
                dao:fields_fieldvars(fields([{Key, Value}]))
            ],
            [{Key, Value}]
        )
    );


get(Con, all, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields),
                <<" from lang">>
            ]
        )
    );

get(Con, _, Fields)->
    get(Con, all, Fields).


create(Con, Proplist)->
    io:format("Proplist = ~p~n", [Proplist]),
    Fields = fields(Proplist),
    dao:pgret(
        dao:equery(Con,[
            <<"insert into lang (">>,
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
                    <<"update  lang set">>,
                        dao:fields_fieldvars(Fields),
                    <<"where id= $id">>
                ],Proplist)
            ),
            {ok, Id}
    end.



%%
%% Local Functions
%%

