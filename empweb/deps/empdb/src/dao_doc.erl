%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_doc).

%%
%% Include files
%%


%%
%% Exported Functions
%%
-export([
    fields/0,
    is_field/1,
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
        title,
        content,
        doc_type_id,
        content_type_id,
        owner_id,
        parent_id
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

get(Con, {title, Name})->
    get(Con, {title, Name}, []);

get(Con, {owner_id, Name})->
    get(Con, {owner_id, Name}, []);

get(Con, {parent_id, Name})->
    get(Con, {parent_id, Name}, []);


get(Con, Some) ->
    get(Con, Some, []).

get(Con, {id, Id}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, all),
                <<" from doc where id = $1">>
            ],
            [Id]
        )
    );

get(Con, {title, Title}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, all),
                <<" from doc where title = $1">>
            ],
            [Title]
        )
    );

get(Con, {owner_id, Owner_id}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, all),
                <<" from doc where owner_id = $1">>
            ],
            [Owner_id]
        )
    );

get(Con, {parent_id, Parent_id}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, all),
                <<" from doc where parent_id = $1">>
            ],
            [Parent_id]
        )
    );

get(Con, {Key, Value}, Fields) when erlang:is_atom(Key) ->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, all),
                <<" from doc where">>,
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
                dao:fields(Fields, all),
                <<" from doc">>
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
            <<"insert into doc (">>,
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
                    <<"update  doc set">>,
                        dao:fields_fieldvars(Fields),
                    <<"where id= $id">>
                ],Proplist)
            ),
            {ok, Id}
    end.



%%
%% Local Functions
%%

