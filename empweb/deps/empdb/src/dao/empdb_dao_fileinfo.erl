%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add descr to biz_user
-module(empdb_dao_fileinfo).
-behaviour(empdb_dao).

%%
%% Include files
%%


-export([
    table/1,
    table/0,
    create/2,
    update/2,
    get/2,
    get/3,
    is_owner/3
]).


%%
%% API Functions
%%


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
        file_id,
        'size',
        path,
        name,
        dir,

        owner_id,
        owner_nick,
        doc_id,

        fileinfotype_id,
        fileinfotype_alias,

        filetype_id,
        filetype_alias,
        
        image_width,
        image_height,

        tokenlong,
        tokenstring,
        md5long,
        md5string,
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
    fileinfo.

table()->
    table(name).


get(Con, Some) ->
   empdb_dao:get(?MODULE, Con, Some).

get(Con, What, Fields)->
    empdb_dao:get(?MODULE, Con, What, Fields).

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

