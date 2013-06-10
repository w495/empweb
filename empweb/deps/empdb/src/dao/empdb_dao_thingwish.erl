%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add descr to biz_user
-module(empdb_dao_thingwish).
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
    count_by_thingtype/2,
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
        owner_id,
        owner_nick,
        thing_id,
        thing_alias,
        thingtype_id,
        thingtype_alias,
        created,
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
    thingwish.

table()->
    table(name).

count(Con, What) ->
    empdb_dao:count(?MODULE, Con, What).


count_by_thingtype(Con, What) ->
    %Expression = [
        %<<"select ">>,
            %<<"count(thingwish.id), ">>,
            %<<"thingwish.thingtype_id, ">>,
            %<<"thingwish.thingtype_alias, ">>,
            %<<"parentthingtype.alias as thingtype_parent_alias, ">>,
            %<<"childthingtype.parent_id as thingtype_parent_id ">>,
        %<<"from thingwish ">>,
            %<<"left join thingtype as childthingtype on ">>,
                %<<"thingwish.thingtype_id = childthingtype.id and ">>,
                %<<"childthingtype.isdeleted = false ">>,
            %<<"left join thingtype as parentthingtype on ">>,
                %<<"parentthingtype.id = childthingtype.parent_id and ">>,
                %<<"parentthingtype.isdeleted = false ">>,
            %<<"where ">>,
                %<<"thingtype_id is null or ">>,
                %<<"( ">>,
                    %<<"thingwish.isdeleted = $isdeleted and ">>,
                    %<<"( ">>,
                        %<<"thingwish.owner_id     =   $owner_id   or  ">>,
                        %<<"thingwish.owner_nick   =   $owner_nick      ">>,
                    %<<") ">>,
                %<<") ">>,
            %<<"group by ">>,
                %<<"thingwish.thingtype_id, ">>,
                %<<"thingwish.thingtype_alias, ">>,
                %<<"thingtype_parent_id, ">>,
                %<<"thingtype_parent_alias;">>
    %],

    Expression = [
        <<"select ">>,
            <<"count(thingwish.id), ">>,
            <<"thingtype.id, ">>,
            <<"parent_id, ">>,
            <<"alias ">>,
        <<"from thingtype ">>,
            <<"left join thingwish on ">>,
                <<"thingwish.thingtype_id = thingtype.id and ">>,
                <<"( ">>,
                    <<"thingwish.isdeleted = $isdeleted and ">>,
                    <<"( ">>,
                        <<"thingwish.owner_id     =   $owner_id   or  ">>,
                        <<"thingwish.owner_nick   =   $owner_nick      ">>,
                    <<") ">>,
                <<") ">>,
        <<"group by ">>,
            <<"thingtype.id, ">>,
            <<"parent_id, ">>,
            <<"alias ">>,
        <<"order by ">>,
            <<"thingtype.id;">>
    ],

    empdb_dao:eqret(
        Con,
        Expression,
        [
            {owner_id,      proplists:get_value(owner_id,   What, null)},
            {owner_nick,    proplists:get_value(owner_nick, What, null)},
            {isdeleted,     proplists:get_value(isdeleted,  What, null)}
        ]
    ).


get(Con, What) ->
    Truefields = proplists:get_value(fields,What,[]),

    Fields =
        case Truefields of
            [] ->
                lists:append([
                    empdb_dao_thing:table({fields, select}),
                    empdb_dao_thingwish:table({fields, select}),
                    [image_width, image_height, file_id, path]
                ]);
            _ ->
                Truefields
        end,

    Req_width     = proplists:get_value(image_width, What, null),
    Req_height    = proplists:get_value(image_height, What, null),


    case empdb_dao:get([
        {empdb_dao_thingwish, thing_id},
        {empdb_dao_thing, {id, file_id}},
        {empdb_dao_file, {left, id}},
        {empdb_dao_fileinfo, {left, file_id}}
    ],Con,[        {fields, [
            fileinfotype_alias,
            'fileinfo.filetype_ext',
            {as, {'fileinfo.path', path}},
            {as, {'fileinfo.dir',  dir}}
            | proplists:delete(path, Fields)
        ]},
        {'or', [
            {fileinfotype_alias, filesystem},
            {'thing.file_id', null}
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


get__(Con, What) ->
    Truefields = proplists:get_value(fields,What,[]),

    Fields =
        case Truefields of
            [] ->
                empdb_dao_thingwish:table({fields, select});
            _ ->
                Truefields
        end,

    case empdb_dao:get([
        {empdb_dao_thingwish, thing_id},
        {empdb_dao_thing, {id, file_id}},
        {empdb_dao_file, {left, id}},
        {empdb_dao_fileinfo, {left, file_id}}
    ],Con,[
        {'or', [
            {fileinfotype_alias, download},
            {'thing.file_id', null}
        ]},
        {fields, [
            {as, {'fileinfo.path', fileinfopath}},
            {as, {'fileinfo.dir',  fileinfodir}}
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

