%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_cdoclot).

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
        case empdb_dao_cdoclot:get(Con,[
            {   filter,
                [
                    {isdeleted, false}
                    |proplists:get_value(filter, Params, [
                        {cdoc_id, proplists:get_value(cdoc_id, Params)}
                    ])
                ]
            }
        ]) of
            {ok, []} ->
                empdb_dao_cdoclot:create(Con, Params);
            {ok, _} ->
                {error, {not_uniq_cdoclot, {[
                    {cdoc_id,
                        proplists:get_value(cdoc_id, Params,
                            proplists:get_value(cdoc_id,
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
        empdb_dao_cdoclot:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_cdoclot:get(
            Con,
            [
                {isdeleted, false}
                |Params
            ] ++ [
                {order, {desc, created}}
            ]
        )
    end).

get(Params, Fileds)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_cdoclot:get(
            Con,
            [
                {isdeleted, false}
                |Params
            ] ++ [
                {order, {desc, created}}
            ],
            Fileds
        )
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_cdoclot:is_owner(Con, Uid, Oid)
    end).

delete(Filter)->
    empdb_dao:with_transaction(fun(Con)->
        {ok, cdoclots} =empdb_dao_cdoclot:update(Con, [
            {values, [{isdeleted, true}]},
            {fields, [
                id,
                cdoc_id,    owner_id,
                dtstart,    dtstop,
                betmin,     betmax
            ]},
            {filter, [{isdeleted, false}|Filter]}
        ]),
        lists:map(
            fun({cdoclotpl}) ->
                cdoclot_owner_id    = proplists:get_value(owner_id, cdoclotpl),
                cdoclot_id          = proplists:get_value(id,       cdoclotpl),
                cdoc_id             = proplists:get_value(cdoc_id,  cdoclotpl),
                {ok, _} =
                    empdb_dao_doc:update(Con, [
                        {id,                cdoc_id},
                        {cdoclot_id,        null},
                        {cdoclot_betmin,    null},
                        {cdoclot_betmax,    null},
                        {cdoclot_dtstart,   null},
                        {cdoclot_dtstop,    null},
                        {cdocbet_id,        null},
                        {cdocbet_owner_id,  null},
                        {cdocbet_owner_nick,null},
                        {cdocbet_price,     null}
                    ]),
                case empdb_dao_cdocbet:get(Con, [
                    {isdeleted, false},
                    {cdoclot_id, cdoclot_id},
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
                                {paytype_alias,     cdocbet_in},
                                {isincome,          true},
                                {price,             Price}
                            ]);
                    _ ->
                        ok
                end
            end,
            cdoclots
        ),
        {ok, cdoclots}
    end).

timeout()->
    remove_expired().

remove_expired()->
    empdb_dao:with_transaction(fun(Con)->
        Now = nowsec(),
        Nowdt = {date(), time()},
        {ok, cdoclots} =
            empdb_dao_cdoclot:update(Con,[
                {filter, [
                    {isdeleted, false},
                    {dtstop, {lt, Nowdt}}
                ]},
                {fields, [
                    id,
                    cdoc_id,    owner_id,
                    dtstart,    dtstop,
                    betmin,     betmax
                ]},
                {values, [
                    {isdeleted, true}
                ]}
            ]),
        lists:map(
            fun({cdoclotpl}) ->
                cdoclot_owner_id    = proplists:get_value(owner_id, cdoclotpl),
                cdoclot_id          = proplists:get_value(id,       cdoclotpl),
                cdoc_id             = proplists:get_value(cdoc_id,  cdoclotpl),
                case empdb_dao_cdocbet:get(Con, [
                    {isdeleted, false},
                    {cdoclot_id, cdoclot_id},
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
                                {id,        cdoclot_owner_id},
                                {money,     {incr, Price}}
                            ]),
                        {ok, _} =
                            empdb_dao_pay:create(Con, [
                                {pers_id,           cdoclot_owner_id},
                                {paytype_alias,     cdoclot_in},
                                {isincome,          true},
                                {price,             Price}
                            ]),
                        {ok, [{cdocpl}]} =
                            empdb_dao_doc:update(Con, [
                                {id,                cdoc_id},
                                {cdoclot_id,        null},
                                {cdoclot_betmin,    null},
                                {cdoclot_betmax,    null},
                                {cdoclot_dtstart,   null},
                                {cdoclot_dtstop,    null},
                                {cdocbet_id,        null},
                                {cdocbet_owner_id,  null},
                                {cdocbet_owner_nick,null},
                                {cdocbet_price,     null},
                                {owner_id,          Owner_id}
                            ]),
                        %% Победителю шлем сообщение, что он победил
                        {ok, _} = empdb_dao_event:create(Con, [
                            {eventobj_alias,    cdocbet},
                            {eventact_alias,    create},
                            {owner_id,          Owner_id},
                            {target_id,         Maxbetpl_id},
                            {pers_id,           cdoclot_owner_id},
                            {eventtype_alias,   create_cdocbet_win}
                        ]),
                        %% Владельцу аукциона шлем сообщение,
                        %% что аукцион окончен
                        empdb_dao_event:create(Con, [
                            {eventobj_alias,    cdoclot},
                            {eventact_alias,    delete},
                            {owner_id,          cdoclot_owner_id},
                            {doc_id,            cdoclot_id},
                            {pers_id,           Owner_id},
                            {eventtype_alias,   delete_cdoclot_expired}
                        ]),
                        {ok, _} =
                            empdb_dao_pers:update(Con, [
                                {id,        Owner_id},
                                {own_cdoc_id,
                                    proplists:get_value(id, cdocpl)},
                                {own_cdoc_head,
                                    proplists:get_value(id, cdocpl)},
                                {citizen_cdoc_id,
                                    proplists:get_value(id, cdocpl)},
                                {citizen_cdoc_head,
                                    proplists:get_value(id, cdocpl)}
                            ]);
                    _ ->
                        %% Владельцу аукциона шлем сообщение,
                        %% что аукцион окончен
                        empdb_dao_event:create(Con, [
                            {eventobj_alias,    cdoclot},
                            {eventact_alias,    delete},
                            {owner_id,          cdoclot_owner_id},
                            {doc_id,            cdoclot_id},
                            {eventtype_alias,   delete_cdoclot_expired}
                        ]),
                        ok
                end
            end,
            cdoclots
        ),
        {ok, cdoclots}
    end).

nowsec() ->
    {Mgs,Sec, _mis} = erlang:now(),
    Now = Mgs * 1000000 + Sec,
    Now.
