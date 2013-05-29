%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_dao_post).
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
    get_adds/2,
    get/2,
    get/3,
    get_top/2
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
        doc_id,
        pic_file_id
%         ncomments
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
    post.

table()->
    table(name).


get(Con, What) ->
    Truefields = proplists:get_value(fields,What,[]),
    Fields =
        case Truefields of
            [] ->
                lists:append([
                    empdb_dao_post:table({fields, select}),
                    empdb_dao_doc:table({fields, select}),
                    [image_width, image_height, file_id, path]
                ]);
            _ ->
                Truefields
        end,

    Req_width     = proplists:get_value(image_width, What, null),
    Req_height    = proplists:get_value(image_height, What, null),

    case empdb_dao:get([
        {empdb_dao_doc, id},
        {empdb_dao_post, {doc_id, pic_file_id}},
        {empdb_dao_file, {left, id}},
        {empdb_dao_fileinfo, {left, file_id}}
    ],Con,[
        {fields, [
            fileinfotype_alias,
            fileinfo.filetype_ext,
            {as, {fileinfo.path, path}},
            {as, {fileinfo.dir,  dir}}
            | proplists:delete(path, Fields)
        ]},
        {'or', [
            {fileinfotype_alias, filesystem},
            {post.pic_file_id, null}
        ]},
        {image_height, null},
        {image_width, null}
        |proplists:delete(fields,
            proplists:delete(image_height,
                proplists:delete(image_width, What)))

    ]) of
        {ok,Phobjs} ->
            {ok,
                lists:map(
                    fun({Phpl})->
                        case (lists:member(pic_file_path, Truefields) or (Truefields =:= []))
                            and (proplists:get_value(path, Phpl) =/= null)
                            and (proplists:get_value(dir, Phpl) =/= null) of
                            true ->
                                empdb_biz_file:get_handle_picture(
                                    Con,
                                    {Phpl},
                                    [
                                        {options, [
                                            {outpathname, pic_file_path}
                                        ]}
                                        |What
                                    ],
                                    Fields,
                                    Req_width,
                                    Req_height
                                );
                            _ ->
                                {[
                                    {pic_file_path, null}
                                    |proplists:delete(fileinfodir, proplists:delete(fileinfopath, Phpl))
                                ]}
                        end
                    end,
                    Phobjs
                )
            };
        Error ->
            Error
    end.


get_top(Con, What) ->
    Truefields = proplists:get_value(fields,What,[]),
    Fields =
        case Truefields of
            [] ->
                lists:append([
                    empdb_dao_post:table({fields, select}),
                    empdb_dao_doc:table({fields, select}),
                    [image_width, image_height, file_id, path]
                ]);
            _ ->
                Truefields
        end,

    Req_width     = proplists:get_value(image_width, What, null),
    Req_height    = proplists:get_value(image_height, What, null),
    Toptime     = proplists:get_value(toptime,      What, week),

    case empdb_dao:get([
        {empdb_dao_doc, id},
        {empdb_dao_post, {doc_id, pic_file_id}},
        {empdb_dao_file, {left, id}},
        {empdb_dao_fileinfo, {left, file_id}},
        {{empdb_dao_vote,  vote},
            {left, {doc_id,   {doc, id}}}}
    ],Con,[
        {fields, [
            {distinct, id},
            fileinfotype_alias,
            fileinfo.filetype_ext,
            {as, {fileinfo.path, path}},
            {as, {fileinfo.dir,  dir}}
            | proplists:delete(
                path,
                proplists:delete(id, Fields)
            )
        ]},
        {'or', [
            {fileinfotype_alias, filesystem},
            {post.pic_file_id, null}
        ]},
        {image_height,      null},
        {image_width,       null},
        {'doc.isdeleted',   false},
        {'doc.isrepost',    false},
        {'doc.isrepostcont',false},
        {order, {desc,  'doc.nvotes'}},
        {'vote.created',
            {gt, empdb_convert:now_minus(Toptime)}
        }
        |proplists:delete(fields,
            proplists:delete(image_height,
                proplists:delete(image_width, What)))

    ]) of
        {ok,Phobjs} ->
            {ok,
                lists:map(
                    fun({Phpl})->
                        case (lists:member(pic_file_path, Truefields) or (Truefields =:= []))
                            and (proplists:get_value(path, Phpl) =/= null)
                            and (proplists:get_value(dir, Phpl) =/= null) of
                            true ->
                                empdb_biz_file:get_handle_picture(
                                    Con,
                                    {Phpl},
                                    [
                                        {options, [
                                            {outpathname, pic_file_path}
                                        ]}
                                        |What
                                    ],
                                    Fields,
                                    Req_width,
                                    Req_height
                                );
                            _ ->
                                {[
                                    {pic_file_path, null}
                                    |proplists:delete(fileinfodir, proplists:delete(fileinfopath, Phpl))
                                ]}
                        end
                    end,
                    Phobjs
                )
            };
        Error ->
            Error
    end.


get__(Con, What) ->

    Truefields = proplists:get_value(fields,What,[]),

    Fields =
        case Truefields of
            [] ->
                lists:append([
                    empdb_dao_post:table({fields, select}),
                    empdb_dao_doc:table({fields, select})
                ]);
            _ ->
                Truefields
        end,

    case empdb_dao:get([
        {empdb_dao_doc, id},
        {empdb_dao_post, {doc_id, pic_file_id}},
        {empdb_dao_file, {left, id}},
        {empdb_dao_fileinfo, {left, file_id}}
    ],Con,[
        {'or', [
            {fileinfotype_alias, download},
            {post.pic_file_id, null}
        ]},
        {fields, [
            {as, {fileinfo.path, fileinfopath}},
            {as, {fileinfo.dir,  fileinfodir}}
            | proplists:delete(pic_file_path, Fields)
        ]}
        |proplists:delete(fields, What)
    ]) of
        {ok,Phobjs} ->
            {ok,
                lists:map(fun({Phpl})->
                    case (lists:member(pic_file_path, Truefields) or (Truefields =:= []))
                        and (proplists:get_value(fileinfodir, Phpl) =/= null)
                        and (proplists:get_value(fileinfopath, Phpl) =/= null) of
                        true ->
                            {[
                                {pic_file_path,
                                    <<  (proplists:get_value(fileinfodir, Phpl))/binary,
                                        (proplists:get_value(fileinfopath, Phpl))/binary
                                    >>
                                }
                                | proplists:delete(fileinfodir,
                                    proplists:delete(fileinfopath,
                                        proplists:delete(path, Phpl)))
                            ]};
                        _ ->
                            {[
                                {pic_file_path, null}
                                |proplists:delete(fileinfodir, proplists:delete(fileinfopath, Phpl))
                            ]}
                    end
                end, Phobjs)
            };
        Error ->
            Error
    end.
%
%     empdb_dao_doc:get(?MODULE, Con, What).
get(Con, What, Afields)->
    get(Con, [{fields, Afields}|What]).

create(Con, Proplist)->
    empdb_dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    empdb_dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    empdb_dao_doc:is_owner(Con, Owner_id, Obj_id).

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


get_adds(Con, Getresult) ->
    case Getresult of
        {ok, List} ->
            {ok, lists:map(fun({Itempl})->
                case proplists:get_value(id, Itempl) of
                    undefined ->
                        {Itempl};
                    Id ->
                        {ok, Comments}     = ?MODULE:count_comments(Con, [{id, Id}]),
                        Ncommentspl = lists:foldl(fun({Commentspl}, Acc)->
                            [{ncomments, proplists:get_value(count, Commentspl)}|Acc]
                        end, [], Comments),
                        {lists:append([Ncommentspl, Itempl])}
                end
            end, List)};
        {Eclass, Error} ->
            {Eclass, Error}
    end.
%%
%% Local Functions
%%

