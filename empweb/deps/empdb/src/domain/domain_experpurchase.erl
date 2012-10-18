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
                    experlack,
                    money
                ]},
                {limit, 1}
            ]),

        Experlack =
            case proplists:get_value(experlack,   Mbbuyerpl,    0) of
                null ->
                    0;
                Val  ->
                    Val
            end,
            
        Exper = proplists:get_value(exper,   Params,    Experlack),
        Price = exper2price(Exper),
        Money = proplists:get_value(money, Mbbuyerpl,   0),


        io:format("Experlack = ~p~n", [Experlack]),
        io:format("Exper = ~p~n", [Exper]),
        io:format("Price = ~p~n", [Price]),
        io:format("Money = ~p~n", [Money]),
         
        case Price =< Money of
            true ->
                Newmoney = Money - Price,
 
                X = dao_pers:update(Con,[
                    {id,    proplists:get_value(id,   Mbbuyerpl)},
                    {exper, {incr, Exper}},
                    {money, Newmoney}
                ]),

                io:format("X = ~p~n", [X]),
                
         
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
