%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_invisbuy).

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
                    money
                ]},
                {limit, 1}
            ]),

        {ok, [{Mbperspl}]} =
            empdb_dao_pers:get(Con, [
                {'or', [
                    {id,    proplists:get_value(pers_id,   Params)},
                    {nick,  proplists:get_value(pers_nick, Params)}
                ]},
                {fields, [
                    id,
                    invistype_level
                ]},
                {limit, 1}
            ]),

        {ok, [{Invistypepl}]} =
            empdb_dao_invistype:get(Con, [
                {'or', [
                    {id,    proplists:get_value(invistype_id,    Params)},
                    {nick,  proplists:get_value(invistype_alias, Params)}
                ]},
                {fields, [
                    invistype_level,
                    id
                ]},
                {limit, 1}
            ]),

        Price = proplists:get_value(price, Invistypepl,     0),
        Money = proplists:get_value(money, Mbbuyerpl,       0),

        Mbinvisbuys = empdb_dao_invisbuy:get(Con, [{isdeleted, false}|Params]),

        Persil =
            case proplists:get_value(invistype_level, Mbperspl, 0) of
                null ->
                    0;
                Pil ->
                    Pil
            end,
                
        Typeil =
            case proplists:get_value(invistype_level, Invistypepl, 0) of
                null ->
                    0;
                Til ->
                    Til
            end,

        case {Mbinvisbuys, Typeil > Persil , Price =< Money} of
            {{ok, []}, false, _} ->
                {error, {wrong_expired, {[
                    {money,     Money},
                    {price,     Price}
                ]}}};
            {{ok, []}, _, true} ->
                {ok, [{Newpers}]} =
                    empdb_dao_pers:update(Con,[
                        {id,
                            proplists:get_value(id,   Mbbuyerpl)
                        },
                        {money,
                            {decr, Price}
                        },
                        {invistype_id,
                            proplists:get_value(invistype_id, Invistypepl)
                        },
                        {fields, [
                            money
                        ]}
                    ]),
                case empdb_dao_invisbuy:create(Con,Params) of
                    {ok, [{Respl}]} ->
                        {ok, _} = empdb_dao_pay:create(Con, [
                            {pers_id,
                                proplists:get_value(buyer_id,   Params)
                            },
                            {paytype_alias,     invisbuy},
                            {isincome,          false},
                            {price,             Price}
                        ]),
                        {ok, [
                            {[
                                {money, Money - Price},
                                {price, Price}
                                |Respl
                            ]}
                        ]};
                    Else ->
                        Else
                end;
            {{ok, []}, _, false} ->
                {error, {not_enough_money, {[
                    {money, Money},
                    {price, Price}
                ]}}};
            {{ok, _}, _, _} ->
                {error, not_uniq_invisbuy};
            {Else, _, _} ->
                Else
        end
    end).


update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_invisbuy:update(Con, Params)
    end).

count(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_invisbuy:count(Con, [{isdeleted, false}|Params])
    end).
    
get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_invisbuy:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_invisbuy:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_invisbuy:is_owner(Con, Uid, Oid)
    end).

