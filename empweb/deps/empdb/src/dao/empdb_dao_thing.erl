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
                empdb_dao_thing:table({fields, select});
            _ ->
                Truefields
        end,

    case empdb_dao:get([
        {empdb_dao_thing,       file_id},
        {empdb_dao_file,        {left, id}},
        {empdb_dao_fileinfo,    {left, file_id}}
    ],Con,[
        {'or', [
            {fileinfotype_alias,    download},
            {thing.file_id,     null}
        ]},
        {image_width,  null},
        {image_height, null},
        {fields, [
            {as, {fileinfo.path, fileinfopath}},
            {as, {fileinfo.dir,  fileinfodir}}
            | proplists:delete(path, Fields)
        ]}
        |proplists:delete(fields, What)
    ]) of
        {ok,Phobjs} ->
            {ok,
                lists:map(fun({Phpl})->
                    case (lists:member(path, Truefields) or (Truefields =:= [])) of
                        true ->
                            {[
                                {path,
                                    case {
                                        proplists:get_value(fileinfodir, Phpl),
                                        proplists:get_value(fileinfopath, Phpl)
                                    } of
                                        {null, _} ->
                                            null;
                                        {_, null} ->
                                            null;
                                        {Fileinfodir, Fileinfopath} ->
                                            <<  (Fileinfodir)/binary,
                                                (Fileinfopath)/binary
                                            >>
                                    end
                                }
                                | proplists:delete(fileinfodir,
                                    proplists:delete(fileinfopath,
                                        proplists:delete(path, Phpl)))
                            ]};
                        _ ->
                            {proplists:delete(fileinfodir,
                                proplists:delete(fileinfopath, Phpl))}
                    end
                end, Phobjs)
            };
        Error ->
            Error
    end.
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

