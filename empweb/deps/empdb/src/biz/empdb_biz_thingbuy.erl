%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_thingbuy).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").


-define(UNIQ_UNDEFINED, undefined_empdb_biz_thingbuy_1368715868302572).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Блоги
%%
-export([
    count/1,
    timeout/0,
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
    empdb_dao:with_transaction(empdb_biz_pers:wfoe(
        fun(Con)->
            create_check(
                Con,
                %% Берем вещь, и смотрим сколько она стоит
                empdb_dao_thing:get(Con, [
                    {'or', [
                        {id,
                            proplists:get_value(
                                thing_id,
                                Params,
                                null
                            )
                        },
                        {alias,
                            proplists:get_value(
                                thing_alias,
                                Params,
                                null
                            )
                        }
                    ]},
                    {fields, [
                        id,
                        file_id,
                        rent,
                        price
                    ]},
                    {limit, 1}
                ]),
                %% Берем покупателя, и смотрим сколько у него денег
                empdb_dao_pers:get(Con, [
                    {'or', [
                        {id,
                            proplists:get_value(
                                buyer_id,
                                Params,
                                null
                            )
                        },
                        {nick,
                            proplists:get_value(
                                buyer_nick,
                                Params,
                                null
                            )
                        }
                    ]},
                    {fields, [
                        id,
                        money
                    ]},
                    {limit, 1}
                ]),
                {
                    case proplists:get_value(owner_id, Params, proplists:get_value(owner_nick,Params,?UNIQ_UNDEFINED)) of
                        ?UNIQ_UNDEFINED ->
                            {ok, []};
                        _ ->
                            %% Берем получателя, и смотрим
                            empdb_dao_pers:get(Con, [
                                {'or', [
                                    {id,
                                        proplists:get_value(
                                            owner_id,
                                            Params,
                                            null
                                        )
                                    },
                                    {nick,
                                        proplists:get_value(
                                            owner_nick,
                                            Params,
                                            ?UNIQ_UNDEFINED
                                        )
                                    }
                                ]},
                                {fields, [
                                    id
                                ]},
                                {limit, 1}
                            ])
                    end,
                    case proplists:get_value(room_id, Params, proplists:get_value(room_head, Params, ?UNIQ_UNDEFINED)) of
                        ?UNIQ_UNDEFINED ->
                            {ok, []};
                        _ ->
                            %% Берем получателя (страну), и смотрим
                            empdb_dao_room:get(Con, [
                                {'or', [
                                    {id,
                                        proplists:get_value(
                                            room_id,
                                            Params,
                                            null
                                        )
                                    },
                                    {head,
                                        proplists:get_value(
                                            room_head,
                                            Params,
                                            ?UNIQ_UNDEFINED
                                        )
                                    }
                                ]},
                                {fields, [
                                    id,
                                    owner_id
                                ]},
                                {limit, 1}
                            ])
                    end,
                    case proplists:get_value(community_id, Params, proplists:get_value(community_head, Params, ?UNIQ_UNDEFINED)) of
                        ?UNIQ_UNDEFINED ->
                            {ok, []};
                        _ ->
                            %% Берем получателя (сообщество), и смотрим
                            empdb_dao_community:get(Con, [
                                {'or', [
                                    {id,
                                        proplists:get_value(
                                            community_id,
                                            Params,
                                            null
                                        )
                                    },
                                    {head,
                                        proplists:get_value(
                                            community_head,
                                            Params,
                                            ?UNIQ_UNDEFINED
                                        )
                                    }
                                ]},
                                {fields, [
                                    id,
                                    owner_id
                                ]},
                                {limit, 1}
                            ])
                    end
                },
                Params
            )
        end,
        [
            {friend_id,
                proplists:get_value(
                    buyer_id,
                    Params,
                    null
                )
            },
            {friend_nick,
                proplists:get_value(
                    buyer_nick,
                    Params,
                    null
                )
            },
            {pers_id,
                proplists:get_value(
                    owner_id,
                    Params,
                    proplists:get_value(
                        buyer_id,
                        Params,
                        null
                    )
                )
            },
            {pers_nick,
                proplists:get_value(
                    owner_nick,
                    Params,
                    proplists:get_value(
                        buyer_id,
                        Params,
                        null
                    )
                )
            }
        ]
    )).

create_check(
    Con,
    {ok, []},
    _,
    _,
    Params
)->
    {error, no_such_thing};

create_check(
    Con,
    _,
    {ok, []},
    _,
    Params
)->
    {error, no_such_buyer};

create_check(
    Con,
    {ok, [{Mbthingpl}]},
    {ok, [{Mbbuyerpl}]},
    {
        {ok, []},
        {ok, []},
        {ok, []}
    },
    Params
)->
    create_do_(
        Con,
        {ok, [{Mbthingpl}]},
        {ok, [{Mbbuyerpl}]},
        [
            {owner_id, proplists:get_value(id,   Mbbuyerpl)}
            |   proplists:delete(owner_id,
                    proplists:delete(owner_nick, Params)
                )
        ]
    );


create_check(
    Con,
    {ok, [{Mbthingpl}]},
    {ok, [{Mbbuyerpl}]},
    {
        {ok, [{Mbownerpl}]},
        {ok, []},
        {ok, []}
    },
    Params
)->
    create_do_(
        Con,
        {ok, [{Mbthingpl}]},
        {ok, [{Mbbuyerpl}]},
        [
            {owner_id, proplists:get_value(id,   Mbownerpl)}
            |   proplists:delete(owner_id,
                    proplists:delete(owner_nick, Params)
                )
        ]
    );


create_check(
    Con,
    {ok, [{Mbthingpl}]},
    {ok, [{Mbbuyerpl}]},
    {
        _,
        {ok, [{Mbroom}]},
        _
    },
    Params
)->
    case create_do_(
        Con,
        {ok, [{Mbthingpl}]},
        {ok, [{Mbbuyerpl}]},
        [
            {room_id, proplists:get_value(id,   Mbroom)}
            |   proplists:delete(room_id,
                    proplists:delete(room_head, Params)
                )
        ]
    ) of
        {ok, Ok}->
            Thfile_id = proplists:get_value(file_id, Mbthingpl),
            Room_id = proplists:get_value(id,   Mbroom),

            {ok, _} =
                empdb_dao_room:update(
                    Con,
                    [
                        {id, Room_id},
                        {back_file_id, Thfile_id}
                    ]
                ),

            {ok, Ok};
        Else ->
            Else
    end;

create_check(
    Con,
    {ok, [{Mbthingpl}]},
    {ok, [{Mbbuyerpl}]},
    {
        _,
        _,
        {ok, [{Mbcommunity}]}
    },
    Params
)->
    case create_do_(
        Con,
        {ok, [{Mbthingpl}]},
        {ok, [{Mbbuyerpl}]},
        [
            {community_id, proplists:get_value(id,   Mbcommunity)}
            |   proplists:delete(community_id,
                    proplists:delete(community_head, Params)
                )
        ]
    ) of
        {ok, Ok}->
            Thfile_id = proplists:get_value(file_id, Mbthingpl),
            community_id = proplists:get_value(id,   Mbcommunity),

            {ok, _} =
                empdb_dao_community:update(
                    Con,
                    [
                        {id, community_id},
                        {back_file_id, Thfile_id}
                    ]
                ),

            {ok, Ok};
        Else ->
            Else
    end.


create_do_(
    Con,
    {ok, [{Mbthingpl}]},
    {ok, [{Mbbuyerpl}]},
    Params
)->
    Now = erlang:universaltime(),
    Nowint  = empdb_convert:datetime2int(Now),
    Expired = proplists:get_value(expired, Params, null),
    Thingprice = proplists:get_value(price, Mbthingpl, null),
    Thingrent = proplists:get_value(rent, Mbthingpl, null),
    Expiredint = empdb_convert:nullable_datetime2int(Expired),

    {Costserror, Costs} =
        case {Expired, Thingprice, Thingrent} of
            {null, null, _} ->
                {error, no_price};
            {null, _, _} ->
                {ok, Thingprice};
            {_, _, null} ->
                {error, no_rent};
            {_, _, _} ->
                expired2price(Con, Thingrent, Nowint, Expiredint)
        end,

    io:format("~n~n~n Costs = ~p ~n~n~n", [Costs]),

    Money = proplists:get_value(money, Mbbuyerpl),
    case {Costserror, Costs} of
        {error, no_price} ->
            {error, {no_price, {[
                {'now',     Nowint},
                {expired,   Expiredint},
                {money,     Money},
                {price,     Thingprice},
                {rent,      Thingrent}
            ]}}};
        {error, no_rent} ->
            {error, {no_rent, {[
                {'now',     Nowint},
                {expired,   Expiredint},
                {money,     Money},
                {price,     Thingprice},
                {rent,      Thingrent}
            ]}}};
        {error, wrong_expired} ->
            {error, {wrong_expired, {[
                {'now',     Nowint},
                {expired,   Expiredint},
                {money,     Money},
                {price,     Thingprice},
                {rent,      Thingrent}
            ]}}};
        {_, Costs} when Costs =< Money ->
            Newmoney = Money - Costs,
            empdb_dao_pers:update(Con,[
                {id,    proplists:get_value(id,   Mbbuyerpl)},
                {money, {decr, Costs}}
            ]),
            case empdb_dao_thingbuy:create(Con,[
                {price,     Thingprice},
                {expired,   Expired},
                {rent,      Thingrent},
                {costs,     Costs}
                |Params
            ]) of
                {ok, [{Respl}]} ->
                    {ok, _} = empdb_dao_pay:create(Con, [
                        {pers_id,           proplists:get_value(id,   Mbbuyerpl)},
                        {paytype_alias,     thing_out},
                        {isincome,          false},
                        {price,             Costs}
                    ]),
                    {ok, _} = empdb_dao_thingwish:update(Con, [
                        {filter, [
                            {'or', [
                                {thing_id,     proplists:get_value(thing_id,   Params)},
                                {thing_alias,  proplists:get_value(thing_alias, Params)}
                            ]},
                            {'or', [
                                {owner_id,    proplists:get_value(owner_id,   Params, null)},
                                {owner_nick,  proplists:get_value(owner_nick, Params, null)}
                            ]},
                            {isdeleted, false}
                        ]},
                        {values, [
                            {isdeleted, true}
                        ]}
                    ]),

                    {ok, [
                        {[
                            {money, Newmoney},
                            {costs, Costs},
                            {price, Thingprice},
                            {rent,  Thingrent}
                            |Respl
                        ]}
                    ]};
                Else ->
                    Else
            end;
        {_, Costs} when Costs > Money  ->
            {error, {not_enough_money, {[
                {'now',     Nowint},
                {expired,   Expiredint},
                {money,     Money},
                {costs,     Costs},
                {price,     Thingprice},
                {rent,      Thingrent}
            ]}}}
    end.


expired2price(Con, Rent, Nowint, Expiredint) ->

    case ((Expiredint - Nowint) >= ?EMPDB_UNIXTIMEDAY) of
        true ->
            Rangeint    = Expiredint - Nowint,
            Rangedays   = Rangeint / ?EMPDB_UNIXTIMEDAY,
            {ok, empdb_convert:to_money(Rent * Rangedays)};
        false ->
            {error, wrong_expired}
    end.



timeout()->
    remove_expired().

remove_expired()->
    empdb_dao:with_transaction(fun(Con)->
        Nowdt = erlang:universaltime(),
        {ok, Dthingbuy} =
            empdb_dao_thingbuy:update(Con,[
                {filter, [
                    {isdeleted, false},
                    {expired, {lt, Nowdt}},
                    {expired, {neq, null}}
                ]},
                {values, [
                    {isdeleted, true}
                ]}
            ]),
        {ok, Dthingbuy}
    end).

update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        case empdb_dao_thingbuy:update(Con, Params) of
            {ok, [{Respl}]} ->
                {ok, _} = empdb_dao_thingwish:update(Con, [
                    {filter, [
                        {'or', [
                            {thing_id,     proplists:get_value(thing_id,    Respl, null)},
                            {thing_alias,  proplists:get_value(thing_alias, Respl, null)}
                        ]},
                        {'or', [
                            {owner_id,    proplists:get_value(owner_id,   Respl, null)},
                            {owner_nick,  proplists:get_value(owner_nick, Respl, null)}
                        ]},
                        {isdeleted, false}
                    ]},
                    {values, [
                        {isdeleted, true}
                    ]}
                ]),
                {ok, [{Respl}]};
            Else ->
                Else
        end
    end).


count(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingbuy:count(Con, [{isdeleted, false}|Params])
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingbuy:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingbuy:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingbuy:is_owner(Con, Uid, Oid)
    end).
