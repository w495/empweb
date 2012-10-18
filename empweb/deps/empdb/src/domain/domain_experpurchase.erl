%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(domain_experpurchase).

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
    dao:with_transaction(fun(Con)->
        %% Берем покупателя, и смотрим сколько у него денег
        io:format("Params = ~p~n", [Params]),
        {ok, [{Mbbuyerpl}]} =
            dao_pers:get(Con, [
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
        Exper = proplists:get_value(exper,   Params,    0),
        Price = exper2price(Exper),
        Money = proplists:get_value(money, Mbbuyerpl,   0),
        case Price =< Money of
            true ->
                Newmoney = Money - Price,
                dao_pers:update(Con,[
                    {id,    proplists:get_value(id,   Mbbuyerpl)},
                    {exper, {incr, proplists:get_value(exper,   Params)}},
                    {money, Newmoney}
                ]),
                case dao_experpurchase:create(Con,[
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

exper2price(Exper) ->
    erlang:abs(0.5 * Exper).

update(Params)->
    dao:with_transaction(fun(Con)->
        dao_experpurchase:update(Con, Params)
    end).

get(Params)->
    dao:with_transaction(fun(Con)->
        dao_experpurchase:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_experpurchase:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    dao:with_transaction(fun(Con)->
        dao_experpurchase:is_owner(Con, Uid, Oid)
    end).
