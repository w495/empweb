%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_roomlot).

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
    delete/1,
    update/1,
    is_owner/2,
    timeout/0
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
    empdb_dao:with_connection(fun(Con)->
        case empdb_dao_roomlot:get(Con,[
            {   filter,
                [
                    proplists:get_value(filter, Params, [
                        {room_id, proplists:get_value(room_id, Params)}
                    ]),
                    {isdeleted, true}
                ]
            }
        ]) of
            {ok, []} ->
                empdb_dao_roomlot:create(Con, Params);
            {ok, _} ->
                {error, {not_uniq_roomlot, {[
                    {room_id,
                        proplists:get_value(room_id, Params,
                            proplists:get_value(room_id,
                                proplists:get_value(filter, Params, [])
                            )
                        )
                    }
                ]}}};
            Elseget ->
                Elseget
        end
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roomlot:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roomlot:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roomlot:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roomlot:is_owner(Con, Uid, Oid)
    end).

delete(Filter)->
    empdb_dao:with_transaction(fun(Con)->
        {ok, Roomlots} =empdb_dao_roomlot:update(Con, [
            {values, [{isdeleted, true}]},
            {fields, [
                id,
                room_id,    owner_id,
                dtstart,    dtstop,
                betmin,     betmax
            ]},
            {filter, [{isdeleted, false}|Filter]}
        ]),
        lists:map(
            fun({Roomlotpl}) ->
                Roomlot_owner_id    = proplists:get_value(owner_id, Roomlotpl),
                Roomlot_id          = proplists:get_value(id,       Roomlotpl),
                Room_id             = proplists:get_value(room_id,  Roomlotpl),
                {ok, _} =
                    empdb_dao_room:update(Con, [
                        {id,                Room_id},
                        {roomlot_id,        null},
                        {roomlot_betmin,    null},
                        {roomlot_betmax,    null},
                        {roomlot_dtstart,   null},
                        {roomlot_dtstop,    null},
                        {roombet_id,        null},
                        {roombet_owner_id,  null},
                        {roombet_owner_nick,null},
                        {roombet_price,     null}
                    ]),
                case empdb_dao_roombet:get(Con, [
                    {isdeleted, false},
                    {roomlot_id, Roomlot_id},
                    {limit, 1},
                    {order, [
                        {desc, price},
                        {asc, created}
                    ]}
                ]) of
                    {ok, [{Maxbetpl}]} ->
                        Owner_id    = proplists:get_value(owner_id, Maxbetpl),
                        Price       = proplists:get_value(price,    Maxbetpl),
                        {ok, _} =
                            empdb_dao_pers:update(Con, [
                                {id,        Owner_id},
                                {money,     {incr, Price}}
                            ]),
                        {ok, _} =
                            empdb_dao_pay:create(Con, [
                                {pers_id,           Owner_id},
                                {paytype_alias,     roombet_in},
                                {isincome,          true},
                                {price,             Price}
                            ]);
                    _ ->
                        ok
                end
            end,
            Roomlots
        ),
        {ok, Roomlots}
    end).

timeout()->
    remove_expired().

remove_expired()->
    empdb_dao:with_transaction(fun(Con)->
        Now = nowsec(),
        Nowdt = {date(), time()},
        {ok, Roomlots} =
            empdb_dao_roomlot:update(Con,[
                {filter, [
                    {isdeleted, false},
                    {dtstop, {lt, Nowdt}}
                ]},
                {fields, [
                    id,
                    room_id,    owner_id,
                    dtstart,    dtstop,
                    betmin,     betmax
                ]},
                {values, [
                    {isdeleted, true}
                ]}
            ]),
        lists:map(
            fun({Roomlotpl}) ->
                Roomlot_owner_id    = proplists:get_value(owner_id, Roomlotpl),
                Roomlot_id          = proplists:get_value(id,       Roomlotpl),
                Room_id             = proplists:get_value(room_id,  Roomlotpl),
                case empdb_dao_roombet:get(Con, [
                    {isdeleted, false},
                    {roomlot_id, Roomlot_id},
                    {limit, 1},
                    {order, [
                        {desc, price},
                        {asc, created}
                    ]}
                ]) of
                    {ok, [{Maxbetpl}]} ->
                        Maxbetpl_id = proplists:get_value(id, Maxbetpl),
                        Owner_id    = proplists:get_value(owner_id, Maxbetpl),
                        Price       = proplists:get_value(price,    Maxbetpl),
                        {ok, _} =
                            empdb_dao_pers:update(Con, [
                                {id,        Roomlot_owner_id},
                                {money,     {incr, Price}}
                            ]),
                        {ok, _} =
                            empdb_dao_pay:create(Con, [
                                {pers_id,           Roomlot_owner_id},
                                {paytype_alias,     roomlot_in},
                                {isincome,          true},
                                {price,             Price}
                            ]),
                        {ok, [{Roompl}]} =
                            empdb_dao_room:update(Con, [
                                {id,                Room_id},
                                {roomlot_id,        null},
                                {roomlot_betmin,    null},
                                {roomlot_betmax,    null},
                                {roomlot_dtstart,   null},
                                {roomlot_dtstop,    null},
                                {roombet_id,        null},
                                {roombet_owner_id,  null},
                                {roombet_owner_nick,null},
                                {roombet_price,     null},
                                {owner_id,          Owner_id}
                            ]),
                        %% Победителю шлем сообщение, что он победил
                        {ok, _} = empdb_dao_event:create(Con, [
                            {eventobj_alias,    roombet},
                            {eventact_alias,    create},
                            {owner_id,          Owner_id},
                            {target_id,         Maxbetpl_id},
                            {pers_id,           Roomlot_owner_id},
                            {eventtype_alias,   create_roombet_win}
                        ]),
                        %% Владельцу аукциона шлем сообщение,
                        %% что аукцион окончен
                        empdb_dao_event:create(Con, [
                            {eventobj_alias,    roomlot},
                            {eventact_alias,    delete},
                            {owner_id,          Roomlot_owner_id},
                            {doc_id,            Roomlot_id},
                            {pers_id,           Owner_id},
                            {eventtype_alias,   delete_roomlot_expired}
                        ]),
                        {ok, _} =
                            empdb_dao_pers:update(Con, [
                                {id,        Owner_id},
                                {own_room_id,
                                    proplists:get_value(id, Roompl)},
                                {own_room_head,
                                    proplists:get_value(id, Roompl)},
                                {citizen_room_id,
                                    proplists:get_value(id, Roompl)},
                                {citizen_room_head,
                                    proplists:get_value(id, Roompl)}
                            ]);
                    _ ->
                        %% Владельцу аукциона шлем сообщение,
                        %% что аукцион окончен
                        empdb_dao_event:create(Con, [
                            {eventobj_alias,    roomlot},
                            {eventact_alias,    delete},
                            {owner_id,          Roomlot_owner_id},
                            {doc_id,            Roomlot_id},
                            {eventtype_alias,   delete_roomlot_expired}
                        ]),
                        ok
                end
            end,
            Roomlots
        ),
        {ok, Roomlots}
    end).

nowsec() ->
    {Mgs,Sec, _mis} = erlang:now(),
    Now = Mgs * 1000000 + Sec,
    Now.
