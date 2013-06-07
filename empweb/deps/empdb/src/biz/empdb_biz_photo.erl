%% @file'empdb_biz_photo.erl'%%          Описание бизнес логики работы с фотографиями.
%%          Фотография это просто документ.
%%
-module(empdb_biz_photo).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").


%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Блоги
%%
-export([
    get_top/1,
    get/1,
    get/2,
    create/1,
    delete/1,
    update/1,
    repost/1
]).


repost(Params)->
    empdb_dao:with_connection(fun(Con)->
        case empdb_biz_doc:repost(
            empdb_dao_photo,
            Con,
            [{fields, [owner_id, id]}|Params]
        ) of
            {ok, [{Postpl}]} ->
                empdb_daowp_event:feedfriends([
                    {eventobj_alias,    photo},
                    {eventact_alias,    repost},
                    {pers_id,           proplists:get_value(owner_id,   Postpl)},
                    {doc_id,            proplists:get_value(id,         Postpl)},
                    {eventtype_alias,   repost_photo}
                ]),
                {ok, [{Postpl}]};
            Else ->
                Else
        end
    end).

create(Params)->
    empdb_dao:with_connection(fun(Con)->
        %% empdb_dao_photo:create(Con, Params)
        case empdb_dao_photo:create(Con, [
            {fields, [
                id, owner_id
            ]}
            |Params
        ]) of
            {ok, [{Postpl}]} ->
                empdb_daowp_event:feedfriends([
                    {eventobj_alias,    photo},
                    {eventact_alias,    repost},
                    {pers_id,           proplists:get_value(owner_id, Postpl)},
                    {doc_id,            proplists:get_value(id,     Postpl)},
                    {eventtype_alias,   create_photo}
                ]),
                {ok, [{Postpl}]};
            Else ->
                Else
        end
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_photo:update(Con, Params)
    end).

get(Params)->
    empdb_biz:nviewsupm(?MODULE, [Params]),
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_photo:get_adds(Con,
            empdb_dao_photo:get(Con, [
                {order, {desc, created}},
                {isdeleted, false}
                |Params
            ]),
            Params
        )
    end).


get_top(Params)->
    empdb_biz:nviewsupm(?MODULE, [Params]),
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_photo:get_adds(Con,
            empdb_dao_photo:get_top(Con, [
                {isdeleted, false}
                |Params
            ]),
            Params
        )
    end).



get(Params, Fields)->
    ?MODULE:get([{fields, Fields}|Params]).

delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_photo:update(Con, [{isdeleted, true}|Params])
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_photo:is_owner(Con, Uid, Oid)
    end).


