%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_zprotbuy).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").

% 
% -define\(EMPDB_BIZ_ZPROTBUY_DAY_COEF,  0.5).
% 

-define(UNIXTIMEWEEK,  604800). % 60*60*24*7.
-define(UNIXTIMEDAY,   86400). % 60*60*24*7.

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Блоги
%%
-export([
    remove_expired/0,
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

datetime2int(Date) ->
    calendar:datetime_to_gregorian_seconds(Date)
    - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).

int2datetime(Int) ->
    calendar:now_to_universal_time({Int div 1000000,Int rem 1000000,0}).

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

        Now = {erlang:date(), erlang:time()},
        Nowint  = datetime2int(Now),
        Rangeint = Nowint + ?UNIXTIMEWEEK,

        io:format("~n~n~n~n                                                     Params = ~p, ~n~n~n~n", [Params]),
        
        Expired = proplists:get_value(expired, Params, int2datetime(Rangeint)),

        Expiredint = datetime2int(Expired),
        
        Price = expired2price(Con, Nowint, Expiredint),
        Money = proplists:get_value(money, Mbbuyerpl,   0),

        Mbzprotbuys = empdb_dao_zprotbuy:get(Con, [{isdeleted, false}|Params]),
        
        case {Mbzprotbuys, Expiredint > Nowint , Price =< Money} of
            {{ok, []}, false, _} ->
                {error, {wrong_expired, {[
                    {'now',     Nowint},
                    {expired,   Expiredint},
                    {money, Money},
                    {price, Price}
                ]}}};
            {{ok, []}, _, true} ->
                {ok, [{Newpers}]} =
                    empdb_dao_pers:update(Con,[
                        {id,    proplists:get_value(id,   Mbbuyerpl)},
                        {money, {decr, Price}},
                        {fields, [
                            money,
                            expired,
                            expiredlack,
                            expiredlackprice,
                            authority_id,
                            authority_alias
                        ]}
                    ]),
                case empdb_dao_zprotbuy:create(Con,[
                    {price, Price}
                    |Params
                ]) of
                    {ok, [{Respl}]} ->
                        {ok, _} = empdb_dao_pay:create(Con, [
                            {pers_id,           proplists:get_value(buyer_id,   Params)},
                            {paytype_alias,     zprotbuy},
                            {isincome,          false},
                            {price,             Price}
                        ]),
                        {ok, [
                            {[
                                {'now',     Nowint},
                                {expired,   Expiredint},
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
                    {expired, Expired},
                    {money, Money},
                    {price, Price}
                ]}}};
            {{ok, _}, _, _} ->
                {error, not_uniq_zprotbuy};
            {Else, _, _} ->
                Else
        end
    end).

expired2price(Con, Nowint, Expiredint) ->
    {ok,[{Servicepl}]} =
        empdb_dao_service:get(
            Con,
            [
                {alias, create_zprotbuy_coef},
                {fields, [price]},
                {limit, 1}
            ]
        ),
    Price = proplists:get_value(price, Servicepl),

    case Expiredint > Nowint of
        true ->
            Rangeint    = Expiredint - Nowint,
            Rangedays   = Rangeint / ?UNIXTIMEDAY,
            erlang:round(erlang:abs(Price * Rangedays) * 100) / 100;
        false ->
            0
    end.



timeout()->
    remove_expired().

remove_expired()->
    empdb_dao:with_transaction(fun(Con)->
        Nowdt = {date(), time()},
        {ok, Dexiles} =
            empdb_dao_zprotbuy:update(Con,[
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
        empdb_dao_zprotbuy:update(Con, Params)
    end).

count(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_zprotbuy:count(Con, [{isdeleted, false}|Params])
    end).
    
get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_zprotbuy:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_zprotbuy:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_zprotbuy:is_owner(Con, Uid, Oid)
    end).
