%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_blog).

%%
%% Include files
%%


%%
%% Exported Functions
%%
-export([
    fields/0,
    all_fields/0,
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
        doc_id,
        default_content_access_type_id,
        default_content_comment_type_id
    ].

all_fields()->
    lists:append(fields(), dao_doc:fields()).


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
                    dao:fields(Fields, all_fields()),
                <<" from doc "
                  " join blog on "
                  " blog.doc_id = doc.id "
                  " where id = $1 ">>
            ],
            [Id]
        )
    );

get(Con, {title, Title}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, all_fields()),
                <<" from doc "
                  " join blog on "
                  " blog.doc_id = doc.id "
                  " where title = $1 ">>
            ],
            [Title]
        )
    );

get(Con, {owner_id, Owner_id}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, all_fields()),
                <<" from doc "
                  " join blog on "
                  " blog.doc_id = doc.id "
                  " where owner_id = $1 ">>
            ],
            [Owner_id]
        )
    );

get(Con, {parent_id, Parent_id}, Fields)->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, all_fields()),
                <<" from doc "
                  " join blog on "
                  " blog.doc_id = doc.id "
                  " where parent_id = $1 ">>
            ],
            [Parent_id]
        )
    );

get(Con, {Key, Value}, Fields) when erlang:is_atom(Key) ->
    dao:pgret(
        dao:equery(Con,
            [
                <<"select ">>,
                dao:fields(Fields, all_fields()),
                <<" from doc "
                  " join blog on "
                  " blog.doc_id = doc.id where ">>,
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
                dao:fields(Fields, all_fields()),
                <<" from doc "
                  " join blog on "
                  " blog.doc_id = doc.id ">>
            ]
        )
    );

get(Con, _, Fields)->
    get(Con, all, Fields).


doc_pl(Proplist)->
    lists:filter(
        fun({Key, Value})->
            dao_doc:is_field(Key)
        end,
        Proplist
    ).

blog_pl(Proplist)->
    lists:filter(
        fun({Key, Value})->
            is_field(Key)
        end,
        Proplist
    ).
        
create(Con, Proplist)->
    case dao_doc:create(Con,doc_pl(Proplist)) of
        {ok, Doc_id} ->
            create_blog(Con,[{doc_id, Doc_id} | blog_pl(Proplist)]);
        {error, Error} ->
            Error
    end.

create_blog(Con, Proplist)->
    Fields = fields(Proplist),
    dao:pgret(
        dao:equery(Con,[
            <<"insert into blog (">>,
                dao:fields(Fields, all_fields()),
            <<") values (">>,
                dao:fieldvars(Fields),
            <<") returning doc_id; ">>
        ],Proplist)
    ).

update(Con, Proplist)->
    case proplists:get_value(id, Proplist) of
        undefined -> 
            create(Con, Proplist);
        Id ->
            update_(Con, Proplist)
    end.

update_(Con, Proplist)->
    case dao_doc:update(Con,doc_pl(Proplist)) of
        {ok, Doc_id} ->
            update_blog(Con, Doc_id, [{doc_id, Doc_id}| blog_pl(Proplist)]);
        {error, Error} ->
            Error
    end.

update_blog(Con, Doc_id, Proplist)->
    Fields = fields(Proplist),
    dao:pgret(
        dao:equery(Con,[
            <<"update  blog set">>,
                dao:fields_fieldvars(Fields),
            <<"where doc_id= $id">>
        ],Proplist)
    ),
    {ok, Doc_id}.


%%
%% Local Functions
%%

