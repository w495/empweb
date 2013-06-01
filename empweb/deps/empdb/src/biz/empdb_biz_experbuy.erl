%% Author: w-495
%% Created: 25.07.2012
-module(empdb_biz_experbuy).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").

%
% -define\(EXPER_COEF,  0.5).
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
        %% Берем покупателя, и смотрим сколько у него денег.
        %%
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
                            proplists:get_value(buyer_id,   Params)
                        )
                    },
                    {nick,
                        proplists:get_value(
                            owner_nick,
                            Params,
                            proplists:get_value(buyer_nick, Params)
                        )
                    }
                ]},
                {fields, [
                    id,
                    experlack
                ]},
                {limit, 1}
            ]),
        %%
        %% Смотрим какой недостаток опыта имеет получатель,
        %% для перехода на следующий уровень.
        %%
        Experlack =
            case proplists:get_value(experlack,   Mbownerpl,    0) of
                null ->
                    0;
                Val  ->
                    Val
            end,
        %%
        %% Смотрим на сколько требуется увеличить опыт.
        %% Если не указан явно, то используется недостаток опыта
        %% до следующего уговня.
        %%
        Exper = proplists:get_value(exper,   Params,    Experlack),

        %%
        %% Вычислем стоймость опыта.
        %%
        Price = exper2price(Con, Exper),

        %%
        %% Смотрим сколько денег у плательщика.
        %%
        Money = proplists:get_value(money, Mbbuyerpl,   0),
        case {Exper, Price =< Money} of
            {0, _} ->
                {error, {wrong_exper, {[
                    {exper, Exper},
                    {money, Money},
                    {price, Price}
                ]}}};
            {_, true} ->
                case empdb_dao_experbuy:create(Con,[
                    {price, Price}
                    |Params
                ]) of
                    {ok, [{Respl}]} ->
                        %%
                        %% Изменим состояние плательшика.
                        %%
                        {ok, [{Newbuyerpl}]} =
                            empdb_dao_pers:update(Con,[
                                {id,    proplists:get_value(id,   Mbbuyerpl)},
                                {money, {decr, Price}},
                                {fields, [
                                    money
                                ]}
                            ]),
                        %%
                        %% Изменим состояние получателя.
                        %%
                        {ok, [{Newownerpl}]} =
                            empdb_dao_pers:update(Con,[
                                {id,    proplists:get_value(id,   Mbownerpl)},
                                {exper,
                                    {
                                        case Exper > 0 of
                                            true ->
                                                incr;
                                            false ->
                                                decr
                                        end,
                                        Exper
                                    }
                                },
                                {fields, [
                                    exper,
                                    experlack,
                                    experlackprice,
                                    authority_id,
                                    authority_alias
                                ]}
                            ]),
                        %%
                        %% Cоздадим информацию о платежеы
                        %%
                        {ok, _} =
                            empdb_dao_pay:create(Con, [
                                {pers_id,           proplists:get_value(id,   Mbbuyerpl)},
                                {paytype_alias,     exper_out},
                                {isincome,          false},
                                {price,             Price}
                            ]),
                        {ok, [
                            {[
                                {authority_alias,
                                    proplists:get_value(
                                        authority_alias,
                                        Newownerpl
                                    )
                                },
                                {authority_id,
                                    proplists:get_value(
                                        authority_id,
                                        Newownerpl
                                    )
                                },
                                {experlackprice,
                                    proplists:get_value(
                                        experlackprice,
                                        Newownerpl
                                    )
                                },
                                {experlack,
                                    proplists:get_value(
                                        experlack,
                                        Newownerpl
                                    )
                                },
                                {exper,
                                    proplists:get_value(
                                        exper,
                                        Newownerpl
                                    )
                                },
                                {money,
                                    proplists:get_value(
                                        money,
                                        Newbuyerpl
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

exper2price(Con, Exper) ->
    {ok,[{Servicepl}]} =
        empdb_dao_service:get(
            Con,
            [
                {alias, create_experbuy_coef},
                {fields, [price]},
                {limit, 1}
            ]
        ),
    Price = proplists:get_value(price, Servicepl),
    erlang:abs(Price * Exper).

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
