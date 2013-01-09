%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_action).

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
%% Блоги
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Params)->
    empdb_dao:with_transaction(fun(Con)->
        {ok, [{Mbactiontypepl}]} =
            empdb_dao_actiontype:get(Con, [
                {'or', [
                    {id,    proplists:get_value(actiontype_id,   Params)},
                    {alias,  proplists:get_value(actiontype_alias, Params)}
                ]},
                {fields, [
                    id,
                    ispaid,
                    price
                ]},
                {limit, 1}
            ]),
        create(
            Con,
            [
                {mbactiontypepl, Mbactiontypepl}
                |Params
            ],
            {ispaid, proplists:get_value(ispaid, Mbactiontypepl)}
        )
    end).

create(Con, Params, {ispaid, true}) ->
    Mbactiontypepl = proplists:get_value(mbactiontypepl, Params),
    {ok, [{Mbownerpl}]} =
        empdb_dao_pers:get(Con, [
            {'or', [
                {id,    proplists:get_value(owner_id,   Params)},
                {nick,  proplists:get_value(owner_nick, Params)}
            ]},
            {fields, [
                id,
                nick,
                money,
                authority_id,
                authority_level
            ]},
            {limit, 1}
        ]),
    Price = proplists:get_value(price, Mbactiontypepl),
    Money = proplists:get_value(money, Mbownerpl),
    case Price =< Money of
        true ->
            Owner_id = proplists:get_value(id,   Mbownerpl),
            {ok, _} = empdb_dao_pay:create(Con, [
                {pers_id,           Owner_id},
                {paytype_alias,     action_out},
                {isincome,          false},
                {price,             Price}
            ]),
            {ok, _} = empdb_dao_pers:update(Con, [
                {filter, [
                    {id, Owner_id}
                ]},
                {values, [
                    {money, {decr, Price}}
                ]}
            ]),
            empdb_dao_action:create(Con, Params);
        false ->
            {error, {not_enough_money, {[
                {money, Money},
                {price, Price}
            ]}}}
    end;

create(Con, Params, {ispaid, false}) ->
    empdb_dao_action:create(Con, Params).


update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_action:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_action:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_action:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_action:is_owner(Con, Uid, Oid)
    end).
