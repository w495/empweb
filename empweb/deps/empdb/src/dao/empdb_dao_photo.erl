%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_dao_photo).
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
    get_adds/3,
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
        path,
        file_id,
        is_cover
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
    photo.

table()->
    table(name).

get(Con, What) ->
    %     <<  "   select  fileinfo.path,fileinfo.dir from doc "
    %         "   join photo on photo.doc_id = doc.id "
    %         "   join file on file.id = photo.file_id "
    %         "   join fileinfo on fileinfo.file_id = file.id "
    %         "   where (fileinfo.fileinfotype_alias "
    %         "       = $`fileinfo.fileinfotype_alias@filter`)"   >>

    Fields =
        proplists:get_value(
            fields,
            What,
            lists:append([
                empdb_dao_photo:table({fields, select}),
                empdb_dao_doc:table({fields, select}),
                [image_width, image_height, file_id]
            ])
        ),

    Req_width     = proplists:get_value(image_width, What, null),
    Req_height    = proplists:get_value(image_height, What, null),
    
    case empdb_dao:get([
        {empdb_dao_doc, id},
        {empdb_dao_photo, {doc_id, file_id}},
        {empdb_dao_file, id},
        {empdb_dao_fileinfo, file_id}
    ],Con,[
        {fields, [
            fileinfotype_alias,
            fileinfo.filetype_ext,
            {as, {fileinfo.path, path}},
            {as, {fileinfo.dir,  dir}}
            | proplists:delete(path, Fields)
        ]},
        {fileinfotype_alias, filesystem},
        {image_height, null},
        {image_width, null}
        
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %         {'or', [
            %             {image_width, Req_width},
            %             {image_width, null}
            %         ]},
            %         {'or', [
            %             {image_height, Req_height},
            %             {image_height, null}
            %         ]}
            % {image_width, proplists:get_value(image_width,      What, null)},
            % {image_height, proplists:get_value(image_height,     What, null)},
            % {fileinfotype_alias, download}
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
        |proplists:delete(fields,
            proplists:delete(image_height,
                proplists:delete(image_width, What)))

    ]) of
        {ok,Phobjs} ->
            {ok, get_handle_objs(Con, Phobjs, What, Fields, Req_width, Req_height)};
        Error ->
            Error
    end.



get_handle_objs(Con, Phobjs, What, Fields, Req_width, Req_height) ->
    lists:map(
        fun(Phobj)->
            get_handle_obj(Con, Phobj, What, Fields, Req_width, Req_height)
        end,
        Phobjs
    ).

get_handle_obj(Con, {Phobjpl}, What, Fields, Req_width, Req_height) ->

    File_id = proplists:get_value(file_id, Phobjpl),
    Req_width     = proplists:get_value(image_width, What, null),
    Req_height    = proplists:get_value(image_height, What, null),
    

    case empdb_dao_fileinfo:get(Con, [
        {file_id, File_id},
        {fileinfotype_alias, download},
        {image_width, Req_width},
        {image_height, Req_height},
        {limit, 1}
    ]) of
        {ok, []} ->
            io:format("~n~n~n !!!!!!!   !!!!!!!! ~n~n~n"),
            Fs_dir   = proplists:get_value(dir,          Phobjpl, <<>>),
            Fs_path  = proplists:get_value(path,         Phobjpl, <<>>),
            File_id  = proplists:get_value(file_id,      Phobjpl, null),
            Doc_id   = proplists:get_value(doc_id,       Phobjpl, null),
            Owner_id = proplists:get_value(owner_id,     Phobjpl, null),
            Ext      = proplists:get_value(filetype_ext, Phobjpl, <<>>),
            {ok, [{Copypl}]} =
                empdb_biz_file:create_copy_worker([
                    {connection,    Con},
                    {fs_path,       Fs_path},
                    {fs_dir,        Fs_dir},
                    {file_id,       File_id},
                    {doc_id,        Doc_id},
                    {owner_id,      Owner_id},
                    {fileextension, Ext},
                    {image_width,   Req_width},
                    {image_height,  Req_height}
                ]),
            get_transform(Copypl, Phobjpl, Fields);
        {ok, [{Respl}]} ->
            io:format("Respl = ~p~n~n", [Respl]),
            
            get_transform(Respl, Phobjpl, Fields);
        Error ->
            Error
    end.

get_transform(Res, Phobjpl, Fields) ->
    Newphobjpl =
        lists:keyreplace(path, 1,
            lists:keyreplace(dir, 1,
                lists:keyreplace(image_height, 1,
                    lists:keyreplace(image_width, 1,
                        Phobjpl,
                        {image_width,
                            proplists:get_value(image_width, Res)
                        }
                    ),
                    {image_height,
                        proplists:get_value(image_height, Res)
                    }
                ),
                {dir,
                    proplists:get_value(dir, Res)
                }
            ),
            {path,
                proplists:get_value(path, Res)
            }
        ),
    get_handle_pl(Newphobjpl, Fields).


get_handle_pl(Phpl, Fields) ->
    io:format("Phpl = ~p ~n~n~n", [Phpl]),

    get_handle_path((lists:member(path, Fields) or (Fields =:= [])), Phpl).

get_handle_pathtuple(Phpl) ->
   %0 io:format("Phpl = ~p ~n~n~n", [Phpl]),
    {path,
        <<  (proplists:get_value(dir, Phpl))/binary,
            (proplists:get_value(path, Phpl))/binary
        >>
    }.

get_handle_pathtuple(Name, Phpl) ->
    {Name,
        <<  (proplists:get_value(dir, Phpl))/binary,
            (proplists:get_value(path, Phpl))/binary
        >>
    }.

get_handle_path_(Phpl) ->
    [
        get_handle_pathtuple(Phpl) | Phpl
    ].

get_handle_path(true, Phpl) ->
    {[
        get_handle_pathtuple(Phpl)
        | proplists:delete(dir,
                    proplists:delete(path,
                        proplists:delete(path,
                            proplists:delete(fs_path_full,
                                proplists:delete(fileextension, Phpl)))))
    ]};

get_handle_path(_, Phpl) ->
    {proplists:delete(dir,
        proplists:delete(path,
            proplists:delete(fs_path_full,
                proplists:delete(fileextension, Phpl))))}.


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
            " and   doc_comment.parent_id      = $id ",
        Params
    ).


get_adds(Con, {ok, List}, Params) ->
    {ok, lists:map(fun({Itempl})->
        case proplists:get_value(id, Itempl)  of
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



get_adds(_con, Result, _params) ->
    Result.
