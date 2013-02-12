%% @file    empdb_dao_album.erl
%%          Описание доступа к базе данных для альбомамов.
%%          Альбом это просто документ.
%% 
-module(empdb_dao_album).
-behaviour(empdb_dao).

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
    count_comments/2,
    count_photos/2,
    count_albums/2,
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
    album.

table()->
    table(name).

get(Con, What) ->
    empdb_dao_doc:get(?MODULE, Con, What).

get(Con, What, Fields)->
    empdb_dao_doc:get(?MODULE, Con, What, Fields).

create(Con, Proplist)->
    empdb_dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    empdb_dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    empdb_dao_doc:is_owner(Con, Owner_id, Obj_id).

%%
%% @doc Подсчет количества комментариев к даному альбому.
%%
count_comments(Con, Params)->
    empdb_dao:eqret(Con,
        " select "
            " count(doc_comment.id) "
        " from "
            " doc as doc_comment "
        " where "
            "       doc_comment.doctype_alias  = 'comment' "
            " and   doc_comment.isdeleted      = false "
            " and   doc_comment.isrepostcont   = false "
            " and   doc_comment.parent_id      = $id ",
        Params
    ).
%%
%% @doc Подсчет количества фотографий даного альбома.
%%
count_photos(Con, Params)->
    empdb_dao:eqret(Con,
        " select "
            " count(doc_photo.id) "
        " from "
            " doc as doc_photo "
        " where "
            "       doc_photo.doctype_alias  = 'photo' "
            " and   doc_photo.isdeleted      = false "
            " and   doc_photo.isrepostcont   = false "
            " and   doc_photo.parent_id      = $id ",
        Params
    ).
%%
%% @doc Подсчет количества вложенных альбомов даного альбома.
%%
count_albums(Con, Params)->
    case empdb_dao:eqret(Con,
        " select "
            " count(doc_album.id), "
            " doc_album.read_acctype_alias "
        " from "
            " doc as doc_album "
        " where "
            "       doc_album.doctype_alias  = 'album' "
            " and   doc_album.isdeleted      = false "
            " and   doc_album.isrepostcont   = false "
            " and   doc_album.parent_id      = $id "
        " group by "
            " doc_album.read_acctype_alias; ",
        Params
    ) of
        {ok, Sqlist} ->
            List1 = lists:map(fun({Itempl})->
                {[
                    {read_acctype_alias, empdb_convert:to_atom(proplists:get_value(read_acctype_alias, Itempl))}
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

get_adds(Con, Getresult) ->
    case Getresult of
        {ok, List1} ->
            {ok, lists:map(fun({Itempl})->
                case proplists:get_value(id, Itempl) of
                    undefined ->
                        {Itempl};
                    Id ->
                        {ok, Albuncnts} =
                            empdb_dao_album:count_albums(Con, [{id, Id}]),
                        {ok, Photos} =
                            empdb_dao_album:count_photos(Con, [{id, Id}]),
                        {ok, Comments} =
                            empdb_dao_album:count_comments(Con, [{id, Id}]),
                        Nalbumspl = lists:foldl(fun({Albunpl}, Acc)->
                            case proplists:get_value(read_acctype_alias, Albunpl) of
                                all ->
                                    [{nalbums, proplists:get_value(count, Albunpl)}
                                        |proplists:delete(nalbums, Acc)
                                    ];
                                public ->
                                    [{npublicalbums, proplists:get_value(count, Albunpl)}
                                        |proplists:delete(npublicalbums, Acc)
                                    ];
                                private ->
                                    [{nprivatealbums, proplists:get_value(count, Albunpl)}
                                        |proplists:delete(nprivatealbums, Acc)
                                    ];
                                protected ->
                                    [{nprotectedalbums, proplists:get_value(count, Albunpl)}
                                        |proplists:delete(nprotectedalbums, Acc)
                                    ];
                                _ ->
                                    Acc
                            end
                        end, [
                            {nalbums, 0},
                            {npublicalbums, 0},
                            {nprivatealbums, 0},
                            {nprotectedalbums, 0}
                        ], Albuncnts),
                        Ncommentspl = lists:foldl(fun({Commentspl}, Acc)->
                            [{ncomments, proplists:get_value(count, Commentspl)}|Acc]
                        end, [], Comments),
                        Nphotospl = lists:foldl(fun({Photospl}, Acc)->
                            [{nphotos, proplists:get_value(count, Photospl)}|Acc]
                        end, [], Photos),
                        {lists:append([
                            Nalbumspl,
                            Ncommentspl,
                            Nphotospl,
                            Itempl
                        ])}
                end
            end, List1)};
        {Eclass, Error} ->
            {Eclass, Error}
    end.
