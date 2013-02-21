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
                        {money, {decr, Price}}
                    ]),
                    case empdb_dao_thingbuy:create(Con,[
                        {price, Price}
                        |Params
                    ]) of
                        {ok, [{Respl}]} ->
                            {ok, _} = empdb_dao_pay:create(Con, [
                                {pers_id,           proplists:get_value(buyer_id,   Params)},
                                {paytype_alias,     thing_out},
                                {isincome,          false},
                                {price,             Price}
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
        end,
        [
            {pers_id,       proplists:get_value(buyer_id,     Params)},
            {pers_nick,     proplists:get_value(buyer_nick,   Params)},
            {friend_id,     proplists:get_value(owner_id,     Params, proplists:get_value(buyer_id,     Params))},
            {friend_nick,   proplists:get_value(owner_nick,   Params, proplists:get_value(buyer_id,     Params))}
        ]
    )).

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
