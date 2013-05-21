%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add descr to biz_user
-module(empdb_dao_thing).
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
    count/2,
    get/2,
    get/3,
    is_owner/3
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
    table({fields, all}) -- [isdeleted];

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
        id,
        alias,
        name_ti,
        descr_ti,
        thingtype_id,
        thingtype_alias,
        price,
        rent,
        file_id,
        isdeleted
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
    thing.

table()->
    table(name).


count(Con, What) ->
    empdb_dao:count(?MODULE, Con, What).


get(Con, What) ->
    Truefields = proplists:get_value(fields,What,[]),

    Fields =
        case Truefields of
            [] ->
                lists:append([
                    empdb_dao_thing:table({fields, select}),
                    [image_width, image_height, file_id, path]
                ]);
            _ ->
                Truefields
        end,


    Req_width     = proplists:get_value(image_width, What, null),
    Req_height    = proplists:get_value(image_height, What, null),


    case empdb_dao:get([
        {empdb_dao_thing,       file_id},
        {empdb_dao_file,        {left, id}},
        {empdb_dao_fileinfo,    {left, file_id}}
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
            {thing.file_id, null}
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
                        case (lists:member(path, Truefields) or (Truefields =:= []))
                            and (proplists:get_value(path, Phpl) =/= null)
                            and (proplists:get_value(dir, Phpl) =/= null) of
                            true ->
                                empdb_biz_file:get_handle_picture(
                                    Con,
                                    {Phpl},
                                    [
                                        {options, [
                                            {outpathname, path}
                                        ]}
                                        |What
                                    ],
                                    Fields,
                                    Req_width,
                                    Req_height
                                );
                            _ ->
                                {[
                                    {path, null}
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
        %{ok,Phobjs} ->
            %{ok,
                %lists:map(fun({Phpl})->
                    %case (lists:member(path, Truefields) or (Truefields =:= [])) of
                        %true ->
                            %{[
                                %{path,
                                    %case {
                                        %proplists:get_value(fileinfodir, Phpl),
                                        %proplists:get_value(fileinfopath, Phpl)
                                    %} of
                                        %{null, _} ->
                                            %null;
                                        %{_, null} ->
                                            %null;
                                        %{Fileinfodir, Fileinfopath} ->
                                            %<<  (Fileinfodir)/binary,
                                                %(Fileinfopath)/binary
                                            %>>
                                    %end
                                %}
                                %| proplists:delete(fileinfodir,
                                    %proplists:delete(fileinfopath,
                                        %proplists:delete(path, Phpl)))
                            %]};
                        %_ ->
                            %{proplists:delete(fileinfodir,
                                %proplists:delete(fileinfopath, Phpl))}
                    %end
                %end, Phobjs)
            %};
        %Error ->
            %Error
    %end.
%     empdb_dao:get(?MODULE, Con, What, Fields).

get(Con, What, Fields)->
    empdb_dao:get(?MODULE, Con, [{fields, Fields}|What]).

create(Con, Proplist)->
    empdb_dao:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    empdb_dao:update(?MODULE, Con, Proplist).

is_owner(Con, Id, Id)->
    true;
is_owner(Con, _, _)->
    false.


%%
%% Local Functions
%%

