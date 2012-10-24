%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_experbuy).

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
        %% Берем покупателя, и смотрим сколько у него денег
        io:format("Params = ~p~n", [Params]),
        {ok, [{Mbbuyerpl}]} =
            empdb_dao_pers:get(Con, [
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

        ?empdb_debug("Experlack = ~p~n", [Experlack]),
        ?empdb_debug("Exper = ~p~n", [Exper]),
        ?empdb_debug("Price = ~p~n", [Price]),
        ?empdb_debug("Money = ~p~n", [Money]),

        case {Exper, Price =< Money} of
            {0, _} ->
                {error, {wrong_exper, {[
                    {exper, Exper},
                    {money, Money},
                    {price, Price}
                ]}}};
            {_, true} ->
                % Newmoney = Money - Price,
                {ok, [{Newpers}]} =
                    empdb_dao_pers:update(Con,[
                        {id,    proplists:get_value(id,   Mbbuyerpl)},
                        {exper, {incr, Exper}},
                        {money, {decr, Price}},
                        {fields, [
                            money,
                            exper,
                            experlack,
                            experlackprice,
                            authority_id,
                            authority_alias
                        ]}
                    ]),
                case empdb_dao_experbuy:create(Con,[
                    {price, Price}
                    |Params
                ]) of
                    {ok, [{Respl}]} ->
                        {ok, [
                            {[
                                {authority_alias,
                                    proplists:get_value(
                                        authority_alias,
                                        Newpers
                                    )
                                },
                                {authority_id,
                                    proplists:get_value(
                                        authority_id,
                                        Newpers
                                    )
                                },
                                {experlackprice,
                                    proplists:get_value(
                                        experlackprice,
                                        Newpers
                                    )
                                },
                                {experlack,
                                    proplists:get_value(
                                        experlack,
                                        Newpers
                                    )
                                },
                                {exper,
                                    proplists:get_value(
                                        exper,
                                        Newpers
                                    )
                                },
                                {money,
                                    proplists:get_value(
                                        money,
                                        Newpers
                                    )
                                },
                                {price, Price}
                                |Respl
                            ]}
                        ]};
                    Else ->
                        Else
                end;
            {_, false} ->
                {error, {not_enough_money, {[
                    {exper, Exper},
                    {money, Money},
                    {price, Price}
                ]}}}
        end
    end).

exper2price(Exper) ->
    erlang:abs(0.5 * Exper).

update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_experbuy:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_experbuy:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_experbuy:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_experbuy:is_owner(Con, Uid, Oid)
    end).
