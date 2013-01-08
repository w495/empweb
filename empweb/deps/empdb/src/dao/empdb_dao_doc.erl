%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_dao_doc).
-behaviour(empdb_dao).

%%
%% Exported Functions
%%
-export([
    table/1,
    table/0,
    create/2,
    update/2,
    get/2,
    get/3,
    count/2,
    count/3,
    is_owner/3
]).

-export([
    create/3,
    update/3,
    get/4,
    count/4
]).

-export([
    get_acctype/2,
    get_acctype/3,
    create_acctype/2,
    update_acctype/2,
    get_doctype/2,
    get_doctype/3,
    create_doctype/2,
    update_doctype/2,
    get_contype/2,
    get_contype/3,
    create_contype/2,
    update_contype/2
]).

-export([
    get_oktype/2,
    get_oktype/3,
    create_oktype/2,
    update_oktype/2
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
    table({fields, all}) -- [id, isdeleted];

%%
%% @doc Возвращает полный список полей таблицы
%%
table({fields, all})->
    [
        id,
        orig_id,
        
        head,
        body,
        read_acctype_id,
        read_acctype_alias,
        comm_acctype_id,
        comm_acctype_alias,
        oktype_id,
        oktype_alias,
        doctype_id,
        doctype_alias,
        contype_id,
        contype_alias,

        owner_id,
        owner_nick,

        orig_owner_id,
        orig_owner_nick,

        isrepost,
        isrepostable,
        isnoticeable,

        parent_id,

        nviews,
        nvotes,

        created,
        nchildren,  %% количество детей
        nnodes,     %% количество потомков
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
    doc.

table()->
    table(name).

count(Con, What) ->
    empdb_dao:count(?MODULE, Con, What).

%%
%% @doc Возвращает экземпляр документа и экземпляр join-наследника.
%%      Наследник должен быть описан в модуле Module.
%%
count(Module, Con, What) when erlang:is_atom(Con) orelse erlang:is_pid(Con) ->
    empdb_dao:count([{?MODULE, id}, {Module, doc_id}], Con, What);


count(Con, What, Fields)->
    empdb_dao:count(?MODULE, Con, What, Fields).

%%
%% @doc Возвращает экземпляр документа и экземпляр join-наследника.
%%      Наследник должен быть описан в модуле Module.
%%
count(Module, Con, What, Fields)->
    empdb_dao:count([{?MODULE, id},{Module, doc_id}], Con, What, Fields).

get(Con, What) ->
    empdb_dao:get(?MODULE, Con, What).

%%
%% @doc Возвращает экземпляр документа и экземпляр join-наследника.
%%      Наследник должен быть описан в модуле Module.
%%
get(Module, Con, What) when erlang:is_atom(Con) orelse erlang:is_pid(Con) ->
%     ?MODULE:update(Con, [
%         {filter, What},
%         {values, [{vcounter, {incr, 1}}]}
%     ]),
    empdb_dao:get([{?MODULE, id}, {Module, doc_id}], Con, What);

%     case empdb_dao:get([{?MODULE, id}, {Module, doc_id}], Con, alias2id_pl(What, [])) of
%         {ok, Doc} ->
%             {ok, id2alias_pl(Doc, [])};
%         Error ->
%             Error 
%     end;

get(Con, What, Fields)->
    empdb_dao:get(?MODULE, Con, What, Fields).

%%
%% @doc Возвращает экземпляр документа и экземпляр join-наследника.
%%      Наследник должен быть описан в модуле Module.
%%
get(Module, Con, What, Fields)->
%     ?MODULE:update(Con, [
%         {filter, What},
%         {values, [{vcounter, {incr, 1}}]}
%     ]),
    empdb_dao:get([{?MODULE, id},{Module, doc_id}], Con, What, Fields).

create(Con, What)->
    empdb_dao:create(?MODULE, Con, What).

update(Con, What)->
    empdb_dao:update(?MODULE, Con, What).

is_owner(Con, {owner_id, Owner_id}, {id, Id})->
    case get(Con, {id, Id}, [owner_id]) of
        {ok, [{[{owner_id, Owner_id}]}]} ->
            true;
        _ ->
            false
    end;

is_owner(Con, Owner_id, Id)->
    is_owner(Con, {owner_id, Owner_id}, {id, Id}).

alias2id_fields(List, Options) ->
    lists:map(fun
        (oktype)->
            oktype_id;
        (contype)->
            contype_id;
        (doctype)->
            doctype_id;
        (X)->
            X
    end,List).

alias2id_pl(Proplist, Options) ->
    lists:map(fun
        ({oktype, Alias})->
            {oktype_id,        empdb_dao:alias2id(oktype, Alias, Options)};
        ({contype, Alias})->
            {contype_id,    empdb_dao:alias2id(contype, Alias, Options)};
        ({doctype, Alias})->
            {doctype_id,    empdb_dao:alias2id(doctype, Alias, Options)};
        ({fields, List}) ->
            {fields,        alias2id_fields(List, Options)};
        (X)->
            X
    end,Proplist).


id2alias_pl(Doc, Options) ->
    lists:map(fun({Proplist})->
        {lists:foldl(fun
            ({oktype_id, Id}, Acc)->
                [   {oktype_id, Id},
                    {oktype,        empdb_dao:id2alias(oktype, Id, Options)}
                    |Acc
                ];
            ({contype_id, Id}, Acc)->
                [   {contype_id, Id},
                    {contype,       empdb_dao:id2alias(contype, Id, Options)}
                    |Acc
                ];
            ({doctype_id, Id}, Acc)->
                [   
                    {doctype_id, Id},
                    {doctype,       empdb_dao:id2alias(doctype, Id, Options)}
                    |Acc
                ];
            (X, Acc)->
                [X|Acc]
        end, [], Proplist)}    
    end,Doc).

%%
%% @doc Создает экземпляр документа и экземпляр join-наследника.
%%      Наследник должен быть описан в модуле Module.
%%
create(Module, Con, Proplist)->
    empdb_dao:create([{?MODULE, id}, {Module, doc_id}], Con, Proplist).

%%
%% @doc Изменяет экземпляр документа и экземпляр join-наследника.
%%      Наследник должен быть описан в модуле Module.
%%
update(Module, Con, Proplist)->
    empdb_dao:update([{?MODULE, id}, {Module, doc_id}], Con, Proplist).


get_acctype(Con, What) ->
    empdb_dao:get(acctype(), Con, What).

get_acctype(Con, What, Fields)->
    empdb_dao:get(acctype(), Con, What, Fields).

create_acctype(Con, Proplist)->
    empdb_dao:get(acctype(), Con, Proplist).

update_acctype(Con, Proplist)->
    empdb_dao:get(acctype(), Con, Proplist).


get_doctype(Con, What) ->
    empdb_dao:get(doctype(), Con, What).

get_doctype(Con, What, Fields)->
    empdb_dao:get(doctype(), Con, What, Fields).

create_doctype(Con, Proplist)->
    empdb_dao:get(doctype(), Con, Proplist).

update_doctype(Con, Proplist)->
    empdb_dao:get(doctype(), Con, Proplist).


get_contype(Con, What) ->
    empdb_dao:get(contype(), Con, What).

get_contype(Con, What, Fields)->
    empdb_dao:get(contype(), Con, What, Fields).

create_contype(Con, Proplist)->
    empdb_dao:get(contype(), Con, Proplist).

update_contype(Con, Proplist)->
    empdb_dao:get(contype(), Con, Proplist).


get_oktype(Con, What) ->
    empdb_dao:get(oktype(), Con, What).

get_oktype(Con, What, Fields)->
    empdb_dao:get(oktype(), Con, What, Fields).

create_oktype(Con, Proplist)->
    empdb_dao:create(oktype(), Con, Proplist).

update_oktype(Con, Proplist)->
    empdb_dao:update(oktype(), Con, Proplist).


oktype() ->
    [
        %% Имя таблицы.
        {{table, name},                       oktype},
        %% Список всех полей.
        {{table, fields, all},                [id, alias, name_ti, isdeleted]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [id, name_ti, alias]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [name_ti, alias]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [name_ti, alias]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].

acctype() ->
    [
        %% Имя таблицы.
        {{table, name},                       acctype},
        %% Список всех полей.
        {{table, fields, all},                [id, alias, name_ti, isdeleted]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [id, name_ti, alias]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [name_ti, alias]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [name_ti, alias]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].

doctype() ->
    [
        %% Имя таблицы.
        {{table, name},                       doctype},
        %% Список всех полей.
        {{table, fields, all},                [id, alias, name_ti, isdeleted]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [name_ti, id, alias]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [name_ti, alias]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [name_ti, alias]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].

contype() ->
    [
        %% Имя таблицы.
        {{table, name},                       contype},
        %% Список всех полей.
        {{table, fields, all},                [id, alias, name_ti, isdeleted]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [id, name_ti, alias]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [name_ti, alias]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [name_ti, alias]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [alias]}
    ].


