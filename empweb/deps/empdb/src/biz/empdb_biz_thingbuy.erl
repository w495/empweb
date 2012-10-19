%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_thingbuy).

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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Покупки
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Params)->
    empdb_dao:with_transaction(fun(Con)->
        %% Берем вещь, и смотрим сколько она стоит
        {ok, [{Mbthingpl}]} =
            empdb_dao_thing:get(Con, [
                {'or', [
                    {id,    proplists:get_value(thing_id,   Params)},
                    {alias,  proplists:get_value(thing_alias, Params)}
                ]},
                {fields, [
                    price
                ]},
                {limit, 1}
            ]),
        %% Берем покупателя, и смотрим сколько у него денег
        {ok, [{Mbbuyerpl}]} =
            empdb_dao_pers:get(Con, [
                {'or', [
                    {id,    proplists:get_value(buyer_id,   Params)},
                    {nick,  proplists:get_value(buyer_nick, Params)}
                ]},
                {fields, [
                    id,
                    money
                ]},
                {limit, 1}
            ]),
        Price = proplists:get_value(price, Mbthingpl),
        Money = proplists:get_value(money, Mbbuyerpl),
        case Price =< Money of
            true ->
                Newmoney = Money - Price,
                empdb_dao_pers:update(Con,[
                    {id,    proplists:get_value(id,   Mbbuyerpl)},
                    {money, Newmoney}
                ]),
                case empdb_dao_thingbuy:create(Con,[
                    {price, Price}
                    |Params
                ]) of
                    {ok, [{Respl}]} ->
                        {ok, [
                            {[
                                {money, Newmoney},
                                {price, Price}
                                |Respl
                            ]}
                        ]};
                    Else ->
                        Else
                end;
            false ->
                {error, {not_enough_money, {[
                    {money, Money},
                    {price, Price}
                ]}}}
        end
    end).

update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingbuy:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingbuy:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingbuy:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingbuy:is_owner(Con, Uid, Oid)
    end).