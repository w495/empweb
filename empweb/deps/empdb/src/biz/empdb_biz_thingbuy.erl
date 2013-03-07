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
    count/1,
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
    empdb_dao:with_transaction(empdb_biz_pers:wfoe(
        fun(Con)->
            %% Берем вещь, и смотрим сколько она стоит
            {ok, [{Mbthingpl}]} =
                empdb_dao_thing:get(Con, [
                    {'or', [
                        {id,    proplists:get_value(thing_id,   Params)},
                        {alias,  proplists:get_value(thing_alias, Params)}
                    ]},
                    {fields, [
                        rent,
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

            Expired = proplists:get_value(expired, Params, null),
            Thingprice = proplists:get_value(price, Mbthingpl, null),
            Thingrent = proplists:get_value(rent, Mbthingpl, null),
            
            {Costserror, Costs} =
                case {Expired, Thingprice, Thingrent} of
                    {null, null, _} ->
                        {{error, no_price}, null};
                    {null, _, _} ->
                        {ok, Thingprice};
                    {_, _, null} ->
                        {{error, no_rent}, null};
                    {_, _, _} ->
                        Expiredint = empdb_convert:datetime2int(Expired),
                        Now = erlang:universaltime(),
                        Nowint  = empdb_convert:datetime2int(Now),
                        {ok, expired2price(Con, Thingrent, Nowint, Expiredint)}
                end,

            Money = proplists:get_value(money, Mbbuyerpl),
            case {Costserror, Costs =< Money} of
                {{error, no_price}, _} ->
                    {error, {no_price, {[
                        {money, Money},
                        {price, Thingprice},
                        {rent,  Thingrent}
                    ]}}};
                {{error, no_rent}, _} ->
                    {error, {no_rent, {[
                        {money, Money},
                        {price, Thingprice},
                        {rent,  Thingrent}
                    ]}}};
                {_, true} ->
                    Newmoney = Money - Costs,
                    empdb_dao_pers:update(Con,[
                        {id,    proplists:get_value(id,   Mbbuyerpl)},
                        {money, {decr, Costs}}
                    ]),
                    case empdb_dao_thingbuy:create(Con,[
                        {price,     Thingprice},
                        {expired,   Expired},
                        {rent,      Thingrent}
                        |Params
                    ]) of
                        {ok, [{Respl}]} ->
                            {ok, _} = empdb_dao_pay:create(Con, [
                                {pers_id,           proplists:get_value(buyer_id,   Params)},
                                {paytype_alias,     thing_out},
                                {isincome,          false},
                                {price,             Costs}
                            ]),
                            {ok, _} = empdb_dao_thingwish:update(Con, [
                                {filter, [
                                    {'or', [
                                        {thing_id,     proplists:get_value(thing_id,   Params)},
                                        {thing_alias,  proplists:get_value(thing_alias, Params)}
                                    ]},
                                    {'or', [
                                        {owner_id,    proplists:get_value(owner_id,   Params, null)},
                                        {owner_nick,  proplists:get_value(owner_nick, Params, null)}
                                    ]},
                                    {isdeleted, false}
                                ]},
                                {values, [
                                    {isdeleted, true}
                                ]}
                            ]),

                            {ok, [
                                {[
                                    {money, Newmoney},
                                    {costs, Costs},
                                    {price, Thingprice},
                                    {rent,  Thingrent}
                                    |Respl
                                ]}
                            ]};
                        Else ->
                            Else
                    end;
                {_, false} ->
                    {error, {not_enough_money, {[
                        {money, Money},
                        {costs, Costs},
                        {price, Thingprice},
                        {rent,  Thingrent}
                    ]}}}
            end
        end,
        [
            {friend_id,   proplists:get_value(buyer_id,     Params)},
            {friend_nick, proplists:get_value(buyer_nick,   Params)},
            {pers_id,     proplists:get_value(owner_id,     Params, proplists:get_value(buyer_id,     Params))},
            {pers_nick,   proplists:get_value(owner_nick,   Params, proplists:get_value(buyer_id,     Params))}
        ]
    )).

expired2price(Con, Rent, Nowint, Expiredint) ->
    case Expiredint > Nowint of
        true ->
            Rangeint    = Expiredint - Nowint,
            Rangedays   = Rangeint / ?EMPDB_UNIXTIMEDAY,
            empdb_convert:to_money(Rent * Rangedays);
        false ->
            0
    end.

    
update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        case empdb_dao_thingbuy:update(Con, Params) of
            {ok, [{Respl}]} ->
                {ok, _} = empdb_dao_thingwish:update(Con, [
                    {filter, [
                        {'or', [
                            {thing_id,     proplists:get_value(thing_id,    Respl, null)},
                            {thing_alias,  proplists:get_value(thing_alias, Respl, null)}
                        ]},
                        {'or', [
                            {owner_id,    proplists:get_value(owner_id,   Respl, null)},
                            {owner_nick,  proplists:get_value(owner_nick, Respl, null)}
                        ]},
                        {isdeleted, false}
                    ]},
                    {values, [
                        {isdeleted, true}
                    ]}
                ]),
                {ok, [{Respl}]};
            Else ->
                Else
        end
    end).


count(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingbuy:count(Con, [{isdeleted, false}|Params])
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
