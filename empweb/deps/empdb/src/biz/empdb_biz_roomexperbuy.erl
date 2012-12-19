%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_roomexperbuy).

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
        {ok, [{Mbroompl}]} =
            empdb_dao_room:get(Con, [
                {'or', [
                    {id,    proplists:get_value(room_id,   Params)},
                    {nick,  proplists:get_value(room_head, Params)}
                ]},
                {fields, [
                    id,
                    experlack,
                    treas
                ]},
                {limit, 1}
            ]),

        Experlack =
            case proplists:get_value(experlack,   Mbroompl,    0) of
                null ->
                    0;
                Val  ->
                    Val
            end,

        Exper = proplists:get_value(exper,   Params,    Experlack),
        Price = exper2price(Exper),
        Treas = proplists:get_value(treas, Mbroompl,   0),

        ?empdb_debug("Experlack = ~p~n", [Experlack]),
        ?empdb_debug("Exper = ~p~n", [Exper]),
        ?empdb_debug("Price = ~p~n", [Price]),
        ?empdb_debug("Treas = ~p~n", [Treas]),

        case {Exper, Price =< Treas} of
            {0, _} ->
                {error, {wrong_exper, {[
                    {exper, Exper}, 
                    {treas, Treas},
                    {price, Price}
                ]}}};
            {_, true} ->
                % Newtreas = Treas - Price,
                {ok, [{Newroom}]} =
                    empdb_dao_room:update(Con,[
                        {id,    proplists:get_value(id,   Mbroompl)},
                        {exper, {incr, Exper}},
                        {treas, {decr, Price}},
                        {fields, [
                            treas,
                            exper,
                            experlack,
                            experlackprice,
                            authority_id,
                            authority_alias
                        ]}
                    ]),
                case empdb_dao_roomexperbuy:create(Con,[
                    {price, Price}
                    |Params
                ]) of
                    {ok, [{Respl}]} ->
                        {ok, _} = empdb_dao_roomtreas:create(Con, [
                            {room_id,           proplists:get_value(room_id,   Params)},
                            {paytype_alias,     exper_out},
                            {isincome,          false},
                            {price,             Price}
                        ]),
                        {ok, [
                            {[
                                {authority_alias,
                                    proplists:get_value(
                                        authority_alias,
                                        Newroom
                                    )
                                },
                                {authority_id,
                                    proplists:get_value(
                                        authority_id,
                                        Newroom
                                    )
                                },
                                {experlackprice,
                                    proplists:get_value(
                                        experlackprice,
                                        Newroom
                                    )
                                },
                                {experlack,
                                    proplists:get_value(
                                        experlack,
                                        Newroom
                                    )
                                },
                                {exper,
                                    proplists:get_value(
                                        exper,
                                        Newroom
                                    )
                                },
                                {treas,
                                    proplists:get_value(
                                        treas,
                                        Newroom
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
                {error, {not_enough_treas, {[
                    {exper, Exper},
                    {treas, Treas},
                    {price, Price}
                ]}}}
        end
    end).

exper2price(Exper) ->
    erlang:abs(0.5 * Exper).

update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roomexperbuy:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roomexperbuy:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roomexperbuy:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roomexperbuy:is_owner(Con, Uid, Oid)
    end).
