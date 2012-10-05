%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(dao_blog).
-behaviour(dao).

%%
%% Include files
%%


%%
%% Exported Functions
%%
-export([
    table/1,
    table/0,
    create/2,
    update/2,
    count_posts/2,
    count_comments/2,
    get/2,
    get/3
]).

%%
%% API Functions
%%


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
        doc_id
%        nposts,
%         npublicposts,
%         nprivateposts,
%         nprotectedposts,
%        ncomments
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
    blog.

table()->
    table(name).

get(Con, What) ->
    dao_doc:get(?MODULE, Con, What).

get(Con, What, Fields)->
    io:format("What, Fields = ~p~n", [{What, Fields}]),
    dao_doc:get(?MODULE, Con, What, Fields).

count_posts(Con, Params)->
    case dao:eqret(Con,
        " select "
            " count(doc_post.id), "
            " doc_post.read_acctype_alias "
        " from "
            " doc as doc_post "
        " where "
            "       doc_post.doctype_alias  = 'post' "
            " and   doc_post.isdeleted      = false "
            " and   doc_post.parent_id      = $id "
        " group by "
            " doc_post.read_acctype_alias; ",
        Params
    ) of
        {ok, List_} ->
            List = lists:map(fun({Itempl})->
                {[
                    {read_acctype_alias, convert:to_atom(proplists:get_value(read_acctype_alias, Itempl))}
                    | proplists:delete(read_acctype_alias, Itempl)
                ]}
                end, List_
            ),
            {ok, [
                {[
                    {read_acctype_alias, all},
                    {count, lists:foldl(fun({Itempl}, Acc)->
                            proplists:get_value(count, Itempl) + Acc
                        end, 0, List
                    )}
                ]}|List
            ]};
        {Eclass, Error} ->
            {Eclass, Error}
    end.
% 
% count_posts_and_comments(Con, Params)->
%     {ok, List} = count_posts(Con, Params)
%     {[{read_acctype_alias,Typ},{count,10}]}
%

count_comments(Con, Params)->
    dao:eqret(Con,
        " select "
            " count(doc_comment.id) "
        " from "
            " doc as doc_comment "
        " join "
            " doc as doc_post "
        " on "
            "       doc_post.id                 = doc_comment.parent_id "
            " and   doc_post.isdeleted          = false "
            " and   doc_comment.isdeleted       = false "
            " and   doc_post.doctype_alias      = 'post' "
            " and   doc_comment.doctype_alias   = 'comment' "
        " where "
            " doc_post.parent_id = $id ",
        Params
    ).

create(Con, Proplist)->
    dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    dao_doc:is_owner(Con, Owner_id, Obj_id).



%%
%% Local Functions
%%

