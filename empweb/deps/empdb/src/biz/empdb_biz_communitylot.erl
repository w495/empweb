%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_communitylot).

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
        case empdb_dao_communitylot:get(Con,[
            {   filter,
                [
                    {isdeleted, false}
                    |proplists:get_value(filter, Params, [
                        {community_id, proplists:get_value(community_id, Params)}
                    ])
                ]
            }
        ]) of
            {ok, []} ->
                empdb_dao_communitylot:create(Con, Params);
            {ok, _} ->
                {error, {not_uniq_communitylot, {[
                    {community_id,
                        proplists:get_value(community_id, Params,
                            proplists:get_value(community_id,
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
        empdb_dao_communitylot:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_communitylot:get(
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
        empdb_dao_communitylot:get(
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
        empdb_dao_communitylot:is_owner(Con, Uid, Oid)
    end).

delete(Filter)->
    empdb_dao:with_transaction(fun(Con)->
        {ok, Communitylots} =empdb_dao_communitylot:update(Con, [
            {values, [{isdeleted, true}]},
            {fields, [
                id,
                community_id,    owner_id,
                dtstart,    dtstop,
                betmin,     betmax
            ]},
            {filter, [{isdeleted, false}|Filter]}
        ]),
        lists:map(
            fun({Communitylotpl}) ->
                Communitylot_owner_id    = proplists:get_value(owner_id, Communitylotpl),
                Communitylot_id          = proplists:get_value(id,       Communitylotpl),
                Community_id             = proplists:get_value(community_id,  Communitylotpl),
                {ok, _} =
                    empdb_dao_community:update(Con, [
                        {id,                Community_id},
                        {communitylot_id,        null},
                        {communitylot_betmin,    null},
                        {communitylot_betmax,    null},
                        {communitylot_dtstart,   null},
                        {communitylot_dtstop,    null},
                        {communitybet_id,        null},
                        {communitybet_owner_id,  null},
                        {communitybet_owner_nick,null},
                        {communitybet_price,     null}
                    ]),
                case empdb_dao_communitybet:get(Con, [
                    {isdeleted, false},
                    {communitylot_id, Communitylot_id},
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
                                {paytype_alias,     communitybet_in},
                                {isincome,          true},
                                {price,             Price}
                            ]);
                    _ ->
                        ok
                end
            end,
            Communitylots
        ),
        {ok, Communitylots}
    end).

timeout()->
    remove_expired().

remove_expired()->
    empdb_dao:with_transaction(fun(Con)->
        Now = nowsec(),
        Nowdt = {date(), time()},
        {ok, Communitylots} =
            empdb_dao_communitylot:update(Con,[
                {filter, [
                    {isdeleted, false},
                    {dtstop, {lt, Nowdt}}
                ]},
                {fields, [
                    id,
                    community_id,    owner_id,
                    dtstart,    dtstop,
                    betmin,     betmax
                ]},
                {values, [
                    {isdeleted, true}
                ]}
            ]),
        lists:map(
            fun({Communitylotpl}) ->
                Communitylot_owner_id    = proplists:get_value(owner_id, Communitylotpl),
                Communitylot_id          = proplists:get_value(id,       Communitylotpl),
                Community_id             = proplists:get_value(community_id,  Communitylotpl),
                case empdb_dao_communitybet:get(Con, [
                    {isdeleted, false},
                    {communitylot_id, Communitylot_id},
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
                                {id,        Communitylot_owner_id},
                                {money,     {incr, Price}}
                            ]),
                        {ok, _} =
                            empdb_dao_pay:create(Con, [
                                {pers_id,           Communitylot_owner_id},
                                {paytype_alias,     communitylot_in},
                                {isincome,          true},
                                {price,             Price}
                            ]),
                        {ok, [{Communitypl}]} =
                            empdb_dao_community:update(Con, [
                                {id,                Community_id},
                                {communitylot_id,        null},
                                {communitylot_betmin,    null},
                                {communitylot_betmax,    null},
                                {communitylot_dtstart,   null},
                                {communitylot_dtstop,    null},
                                {communitybet_id,        null},
                                {communitybet_owner_id,  null},
                                {communitybet_owner_nick,null},
                                {communitybet_price,     null},
                                {owner_id,          Owner_id}
                            ]),
                        %% Победителю шлем сообщение, что он победил
                        {ok, _} = empdb_dao_event:create(Con, [
                            {eventobj_alias,    communitybet},
                            {eventact_alias,    create},
                            {owner_id,          Owner_id},
                            {target_id,         Maxbetpl_id},
                            {pers_id,           Communitylot_owner_id},
                            {eventtype_alias,   create_communitybet_win}
                        ]),
                        %% Владельцу аукциона шлем сообщение,
                        %% что аукцион окончен
                        empdb_dao_event:create(Con, [
                            {eventobj_alias,    communitylot},
                            {eventact_alias,    delete},
                            {owner_id,          Communitylot_owner_id},
                            {doc_id,            Communitylot_id},
                            {pers_id,           Owner_id},
                            {eventtype_alias,   delete_communitylot_expired}
                        ]),
                        {ok, _} =
                            empdb_dao_pers:update(Con, [
                                {id,        Owner_id},
                                {own_community_id,
                                    proplists:get_value(id, Communitypl)},
                                {own_community_head,
                                    proplists:get_value(id, Communitypl)},
                                {citizen_community_id,
                                    proplists:get_value(id, Communitypl)},
                                {citizen_community_head,
                                    proplists:get_value(id, Communitypl)}
                            ]);
                    _ ->
                        %% Владельцу аукциона шлем сообщение,
                        %% что аукцион окончен
                        empdb_dao_event:create(Con, [
                            {eventobj_alias,    communitylot},
                            {eventact_alias,    delete},
                            {owner_id,          Communitylot_owner_id},
                            {doc_id,            Communitylot_id},
                            {eventtype_alias,   delete_communitylot_expired}
                        ]),
                        ok
                end
            end,
            Communitylots
        ),
        {ok, Communitylots}
    end).

nowsec() ->
    {Mgs,Sec, _mis} = erlang:now(),
    Now = Mgs * 1000000 + Sec,
    Now.
