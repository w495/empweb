%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user

-module(empdb_biz_pptrans).

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
        case empdb_dao_pptrans:create(Con, Params) of
            {ok, Res} ->
                {ok, [{Mbperspl}]} =
                    empdb_dao_pers:get(
                            Con,
                            [
                                {'or', [
                                    {id,
                                        proplists:get_value(pers_id, Params)
                                    },
                                    {nick,
                                        proplists:get_value(pers_nick, Params)
                                    }
                                ]},
                                {fields, [
                                    id,
                                    money
                                ]},
                                {limit, 1}
                            ]
                    ),

                {ok, [{Mbreceiverpl}]} =
                    empdb_dao_pers:get(
                            Con,
                            [
                                {id,
                                    proplists:get_value(receiver_id, Params)
                                },
                                {fields, [
                                    id,
                                    money
                                ]},
                                {limit, 1}
                            ]
                    ),

                Pers_id     = proplists:get_value(id,   Mbperspl),
                Receiver_id = proplists:get_value(id,   Mbreceiverpl),

                Mbownermoney   = proplists:get_value(money,  Mbperspl),
                Price   = proplists:get_value(price,  Params, 1.0),

                case Price =< Mbownermoney of
                    true ->
                        %%
                        %% Создаем платеж пользователя.
                        %% Запись о списании денег у плательщика.
                        %%
                        {ok, _} =
                            empdb_dao_pay:create(Con, [
                                {pers_id,           Pers_id},
                                {paytype_alias,     pers_out},
                                {isincome,          false},
                                {price,             Price}
                            ]),

                        %%
                        %% Создаем платеж пользователя.
                        %% Запись о начислении денег получателю.
                        %%
                        {ok, _} =
                            empdb_dao_pay:create(Con, [
                                {pers_id,           Receiver_id},
                                {paytype_alias,     pers_in},
                                {isincome,          true},
                                {price,             Price}
                            ]),
                        %%
                        %% Списание денег у плательщика.
                        %%
                        {ok, _} =
                            empdb_dao_pers:update(Con, [
                                {values, [
                                    {money, {decr, Price}}
                                ]},
                                {filter, [
                                    {id, Pers_id}
                                ]}
                            ]),
                        %%
                        %% Начислении денег получателю.
                        %%
                        {ok, _} =
                            empdb_dao_pers:update(Con, [
                                {values, [
                                    {money, {incr, Price}}
                                    |
                                    case proplists:get_value(isforwish,   Params) of
                                        true ->
                                            {ok, _} =
                                                empdb_dao_moneywish:update(Con, [
                                                    {filter, [
                                                        {money,     Price},
                                                        {owner_id,  Receiver_id},
                                                        {isdeleted, false}
                                                    ]},
                                                    {values, [
                                                        {isdeleted, true}
                                                    ]}
                                                ]),
                                            [
                                                {moneywish,
                                                    {
                                                        case Price > 0 of
                                                            true ->
                                                                decr;
                                                            false ->
                                                                incr
                                                        end,
                                                        erlang:abs(Price)
                                                    }
                                                }
                                            ];
                                        _ ->
                                            []
                                    end
                                ]},
                                {filter, [
                                    {id, Receiver_id}
                                ]}
                            ]),
                        {ok, Res};
                    false ->
                        {error, {not_enough_money, {[
                            {money, Mbownermoney},
                            {price, Price}
                        ]}}}
                end;
            Error ->
                Error
        end
    end).





update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_pptrans:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_pptrans:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_pptrans:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_pptrans:is_owner(Con, Uid, Oid)
    end).
