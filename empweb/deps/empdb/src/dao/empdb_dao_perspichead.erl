%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_dao_perspichead).
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
        doc_id,
        ismale,
        x,
        y,
        file_id
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
    perspichead.

table()->
    table(name).

get(Con, What) ->
    %     <<  "   select'fileinfo.path','fileinfo.dir'from doc "
    %         "   join perspichead on'perspichead.doc_id'='doc.id'"
    %         "   join file on'file.id'='perspichead.file_id'"
    %         "   join fileinfo on'fileinfo.file_id'='file.id'"
    %         "   where ('fileinfo.fileinfotype_alias'"
    %         "       = $`'fileinfo.fileinfotype_alias'@filter`)"   >>

    Truefields = proplists:get_value(fields,What,[]),

    Fields =
        case Truefields of
            [] ->
                lists:append([
                    empdb_dao_perspichead:table({fields, select}),
                    empdb_dao_doc:table({fields, select})
                ]);
            _ ->
                Truefields
        end,

    case empdb_dao:get([
        {empdb_dao_doc, id},
        {empdb_dao_perspichead, {doc_id, file_id}},
        {empdb_dao_file, id},
        {empdb_dao_fileinfo, file_id}
    ],Con,[
        {fileinfotype_alias, download},
        {fields, [
            {as, {'fileinfo.path', fileinfopath}},
            {as, {'fileinfo.dir',  fileinfodir}}
            | proplists:delete(path, Fields)
        ]},
        {image_width,  null}, 
        {image_height, null}
        |proplists:delete(fields, What)
    ]) of
        {ok,Phobjs} ->
            {ok,
                lists:map(fun({Phpl})->
                    case (lists:member(path, Truefields) or (Truefields =:= [])) of
                        true ->
                            {[
                                {path,
                                    <<  (proplists:get_value(fileinfodir, Phpl))/binary,
                                        (proplists:get_value(fileinfopath, Phpl))/binary
                                    >>
                                }
                                | proplists:delete(fileinfodir,
                                    proplists:delete(fileinfopath,
                                        proplists:delete(path, Phpl)))
                            ]};
                        _ ->
                            {proplists:delete(fileinfodir, proplists:delete(fileinfopath, Phpl))}
                    end
                end, Phobjs)
            };
        Error ->
            Error
    end.

    %empdb_dao_doc:get(?MODULE, Con, What).

get(Con, What, Truefields)->

    Fields =
        case Truefields of
            [] ->
                lists:append([
                    empdb_dao_perspichead:table({fields, select}),
                    empdb_dao_doc:table({fields, select})
                ]);
            _ ->
                Truefields
        end,

    case empdb_dao:get([
        {empdb_dao_doc, id},
        {empdb_dao_perspichead, {doc_id, file_id}},
        {empdb_dao_file, id},
        {empdb_dao_fileinfo, file_id}
    ],Con,[
        {fileinfotype_alias, download}
        |What
    ], [
        {as, {'fileinfo.path', fileinfopath}},
        {as, {'fileinfo.dir',  fileinfodir}}
        | proplists:delete(path, Fields) 
    ]) of
        {ok,Phobjs} ->
            {ok,
                lists:map(fun({Phpl})->
                    case (lists:member(path, Truefields) or (Truefields =:= [])) of
                        true ->
                            {[
                                {path,
                                    <<  (proplists:get_value(fileinfodir, Phpl))/binary,
                                        (proplists:get_value(fileinfopath, Phpl))/binary
                                    >>
                                }
                                | proplists:delete(fileinfodir,
                                    proplists:delete(fileinfopath,
                                        proplists:delete(path, Phpl)))
                            ]};
                        _ ->
                            {proplists:delete(fileinfodir, proplists:delete(fileinfopath, Phpl))}
                    end
                end, Phobjs)
            };
        Error ->
            Error
    end.

    %empdb_dao_doc:get(?MODULE, Con, What).

create(Con, Proplist)->
    empdb_dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    empdb_dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    empdb_dao_doc:is_owner(Con, Owner_id, Obj_id).

count_comments(Con, Params)->
    empdb_dao:eqret(Con,
        " select "
            " count('doc_comment.id') "
        " from "
            " doc as doc_comment "
        " where "
            "'doc_comment.doctype_alias'= 'comment' "
            " and'doc_comment.isdeleted'= false "
            " and'doc_comment.parent_id'= $id ",
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
