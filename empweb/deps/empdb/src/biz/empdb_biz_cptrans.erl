%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user

-module(empdb_biz_cptrans).

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
    get/1,
    get/2,
    create/1,
    update/1
]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          ЗНАЧИМЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




create(Params)->
    empdb_dao:with_transaction(fun(Con)->
        %% Создаем денежный перевод
        case empdb_dao_cptrans:create(Con, Params) of
            {ok, Res} ->
                Pers_id = proplists:get_value(pers_id, Params),
                {ok, [{[{own_room_id, Def_room_id}]}]}
                    = empdb_dao_pers:get(
                            Con,
                            [   {id,    Pers_id},
                                {fields, [own_room_id]}
                            ]
                    ),
                Price   = proplists:get_value(price, Params, 1.0),
                Room_id = proplists:get_value(room_id,Params,Def_room_id),
                %% Создаем платеж пользователя
                {ok, _} = empdb_dao_pay:create(Con, [
                    {pers_id,           Pers_id},
                    {paytype_alias,     room_out},
                    {isincome,          false},
                    {price,             Price}
                ]),
                %% Создаем платеж комнаты
                {ok, _} = empdb_dao_roomtreas:create(Con, [
                    {pers_id,           Pers_id},
                    {room_id,           Room_id},
                    {isincome,          true},
                    {treastype_alias,   in},
                    {price,             Price}
                ]),
                %% Обновляем комнату
                {ok, _} = empdb_dao_room:update(Con, [
                    {values, [
                        {treas, {incr, Price}}
                    ]},
                    {filter, [
                        {id, Room_id}
                    ]}
                ]),
                {ok, _} = empdb_dao_pers:update(Con, [
                    {values, [
                        {money, {decr, Price}}
                    ]},
                    {filter, [
                        {id, Pers_id}
                    ]}
                ]),
                {ok, Res};
            Error ->
                Error
        end
    end).





update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_cptrans:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_cptrans:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_cptrans:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_cptrans:is_owner(Con, Uid, Oid)
    end).
