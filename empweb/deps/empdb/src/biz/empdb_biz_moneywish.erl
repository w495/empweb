%% Author: w-495
%% Created: 25.07.2012
-module(empdb_biz_moneywish).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").

%
% -define\(money_COEF,  0.5).
%

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
        %%
        %% Берем получателя, и смотрим сколько у опыта
        %% и недостатка опыта
        %%
        {ok, [{Mbownerpl}]} =
            empdb_dao_pers:get(Con, [
                {'or', [
                    {id,
                        proplists:get_value(
                            owner_id,
                            Params,
                            proplists:get_value(owner_id,   Params)
                        )
                    },
                    {nick,
                        proplists:get_value(
                            owner_nick,
                            Params,
                            proplists:get_value(owner_nick, Params)
                        )
                    }
                ]},
                {fields, [
                    id
                ]},
                {limit, 1}
            ]),
        Money = proplists:get_value(money, Params,   0),
        Price = Money,
        case money of
            0 ->
                {error, {wrong_money, {[
                    {money, money},
                    {money, Money},
                    {price, Price}
                ]}}};
            _ ->
                case empdb_dao_moneywish:create(Con,[
                    {price, Price}
                    |Params
                ]) of
                    {ok, [{Respl}]} ->
                        %%
                        %% Изменим состояние получателя.
                        %%
                        {ok, [{Newownerpl}]} =
                            empdb_dao_pers:update(Con,[
                                {id,    proplists:get_value(id,   Mbownerpl)},
                                {moneywish,
                                    {
                                        case Money > 0 of
                                            true ->
                                                incr;
                                            false ->
                                                decr
                                        end,
                                        erlang:abs(Money)
                                    }
                                },
                                {fields, [
                                    money,
                                    moneylack,
                                    moneywish,
                                    moneylackprice,
                                    authority_id,
                                    authority_alias
                                ]}
                            ]),
                        {ok, [
                            {[
                                {moneywish,
                                    proplists:get_value(
                                        moneywish,
                                        Newownerpl
                                    )
                                },
                                {money,
                                    proplists:get_value(
                                        money,
                                        Newownerpl
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
                    {money, Money},
                    {price, Price}
                ]}}}
        end
    end).

update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_moneywish:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_moneywish:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_moneywish:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_moneywish:is_owner(Con, Uid, Oid)
    end).
