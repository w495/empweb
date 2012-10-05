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
    get_adds/2,
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
        {ok, Sqlist} ->
            List1 = lists:map(fun({Itempl})->
                {[
                    {read_acctype_alias, convert:to_atom(proplists:get_value(read_acctype_alias, Itempl))}
                    | proplists:delete(read_acctype_alias, Itempl)
                ]}
                end, Sqlist
            ),
            {ok, [
                {[
                    {read_acctype_alias, all},
                    {count, lists:foldl(fun({Itempl}, Acc)->
                            proplists:get_value(count, Itempl) + Acc
                        end, 0, List1
                    )}
                ]}|List1
            ]};
        {Eclass, Error} ->
            {Eclass, Error}
    end.
% 
% count_posts_and_comments(Con, Params)->
%     {ok, List1} = count_posts(Con, Params)
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

get_adds(Con, Getresult) ->
    case Getresult of
        {ok, List1} ->
            {ok, lists:map(fun({Itempl})->
                case proplists:get_value(id, Itempl) of
                    undefined ->
                        {Itempl};
                    Id ->
                        {ok, Postcnts}     = dao_blog:count_posts(Con, [{id, Id}]),
                        {ok, Comments}     = dao_blog:count_comments(Con, [{id, Id}]),
                        Npostspl = lists:foldl(fun({Postpl}, Acc)->
                            case proplists:get_value(read_acctype_alias, Postpl) of
                                all ->
                                    [{nposts, proplists:get_value(count, Postpl)}
                                        |proplists:delete(nposts, Acc)
                                    ];
                                public ->
                                    [{npublicposts, proplists:get_value(count, Postpl)}
                                        |proplists:delete(npublicposts, Acc)
                                    ];
                                private ->
                                    [{nprivateposts, proplists:get_value(count, Postpl)}
                                        |proplists:delete(nprivateposts, Acc)
                                    ];
                                protected ->
                                    [{nprotectedposts, proplists:get_value(count, Postpl)}
                                        |proplists:delete(nprotectedposts, Acc)
                                    ];
                                _ ->
                                    Acc
                            end
                        end, [
                            {nposts, 0},
                            {npublicposts, 0},
                            {nprivateposts, 0},
                            {nprotectedposts, 0}
                        ], Postcnts),
                        Ncommentspl = lists:foldl(fun({Commentspl}, Acc)->
                            [{ncomments, proplists:get_value(count, Commentspl)}|Acc]
                        end, [], Comments),
                        {lists:append([Npostspl, Ncommentspl, Itempl])}
                end
            end, List1)};
        {Eclass, Error} ->
            {Eclass, Error}
    end.

create(Con, Proplist)->
    dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    dao_doc:is_owner(Con, Owner_id, Obj_id).



%%
%% Local Functions
%%

