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
    timeout/0,
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
                    {id,    proplists:get_value(owner_id,   Params)},
                    {nick,  proplists:get_value(owner_nick, Params)}
                ]},
                {fields, [
                    id,
                    invistype_level,
                    invistype_alias
                ]},
                {limit, 1}
            ]),

        {ok, [{Invistypepl}]} =
            empdb_dao_invistype:get(Con, [
                {'or', [
                    {id,        proplists:get_value(invistype_id,    Params)},
                    {alias,     proplists:get_value(invistype_alias, Params)}
                ]},
                {fields, [
                    level,
                    alias,
                    price,
                    id
                ]},
                {limit, 1}
            ]),

        Now = calendar:local_time_to_universal_time({erlang:date(), erlang:time()}),
        Nowint  = empdb_convert:datetime2int(Now),
        Rangeint = Nowint + ?EMPDB_UNIXTIMEWEEK,
        Expired =
            proplists:get_value(
                expired,
                Params,
                empdb_convert:int2datetime(Rangeint)
            ),
        Expiredint = empdb_convert:datetime2int(Expired),
        Price =
            expired2price(
                Con,
                Nowint,
                Expiredint,
                proplists:get_value(price, Invistypepl, 0)
            ),
        Money = proplists:get_value(money, Mbbuyerpl, 0),
        Mbinvisbuys = empdb_dao_invisbuy:get(Con, [{isdeleted, false}|Params]),
        Persil =
            case proplists:get_value(invistype_level, Mbperspl, 0) of
                null ->
                    0;
                Pil ->
                    Pil
            end,
        Typeil =
            case proplists:get_value(level, Invistypepl, 0) of
                null ->
                    0;
                Til ->
                    Til
            end,

        Persal = proplists:get_value(invistype_alias, Mbperspl,    null),
        Typeal = proplists:get_value(alias,           Invistypepl, null),
 
        case {Mbinvisbuys, Typeil > Persil, Expiredint > Nowint, Price =< Money} of
            {{ok, []}, false, true, true} ->
                {error, {wrong_invistype, {[
                    {old_invistype_alias,   Persal},
                    {new_invistype_alias,   Typeal},
                    {old_invistype_level,   Persil},
                    {new_invistype_level,   Typeil},
                    {money,                 Money},
                    {price,                 Price}
                ]}}};
            {{ok, []}, true, false, true} ->
                {error, {wrong_expired, {[
                    {'now',     Nowint},
                    {expired,   Expiredint},
                    {money,     Money},
                    {price,     Price}
                ]}}};
            {{ok, []}, true, true, true} ->
                {ok, [{Newbuyer}]} =
                    empdb_dao_pers:update(Con,[
                        {id,
                            proplists:get_value(id,   Mbbuyerpl)
                        },
                        {money,
                            {decr, Price}
                        },
                        {fields, [
                            money
                        ]}
                    ]),
                {ok, [{Newpers}]} =
                    empdb_dao_pers:update(Con,[
                        {id,
                            proplists:get_value(id,   Mbperspl)
                        },
                        {invistype_id,
                            proplists:get_value(id, Invistypepl)
                        }
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
                                {invistype_id,  proplists:get_value(id, Invistypepl)},
                                {money, Money - Price},
                                {price, Price}
                                |Respl
                            ]}
                        ]};
                    Else ->
                        Else
                end;
            {{ok, []}, true, false, false} ->
                {error, {not_enough_money, {[
                    {expired, Expiredint},
                    {money, Money},
                    {price, Price}
                ]}}};
            {{ok, _}, _, _, _} ->
                {error, not_uniq_invisbuy};
            {Else, _, _, _} ->
                Else
        end
    end).

expired2price(Con, Nowint, Expiredint, Price) ->
    case Expiredint > Nowint of
        true ->
            Rangeint    = Expiredint - Nowint,
            Rangedays   = Rangeint / ?EMPDB_UNIXTIMEDAY,
            empdb_convert:to_money(Price * Rangedays);
        false ->
            0
    end.




timeout()->
    remove_expired().

remove_expired()->
    empdb_dao:with_transaction(fun(Con)->
        Nowdt = calendar:local_time_to_universal_time({erlang:date(), erlang:time()}),
        {ok, Dexiles} =
            empdb_dao_invisbuy:update(Con,[
                {filter, [
                    {isdeleted, false},
                    {expired, {lt, Nowdt}}
                ]},
                {values, [
                    {isdeleted, true}
                ]}
            ]),
        {ok, Dexiles}
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

