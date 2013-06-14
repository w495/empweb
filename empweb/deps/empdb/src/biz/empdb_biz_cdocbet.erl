%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_cdocbet).

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

nowsec() ->
    {Mgs,Sec, _mis} = erlang:now(),
    Now = Mgs * 1000000 + Sec,
    Now.

create(Params)->
    empdb_dao:with_transaction(fun(Con)->
        Cdoclot_id  = proplists:get_value(cdoclot_id, Params),
        Cdocbet_owner_id   = proplists:get_value(owner_id, Params),
        Price              = proplists:get_value(price, Params, 0),
        Now                = nowsec(),
        case {
            %%
            %% Выясним информацию о покупателе и о лоте.
            %%
            empdb_dao_cdoclot:get(Con, [
                {isdeleted, false},
                {id, Cdoclot_id},
                {fields, [
                    cdoc_id,
                    owner_id,
                    dtstart,
                    dtstop,
                    betmin,
                    betmax
                ]},
                {limit, 1}
            ]),
            empdb_dao_pers:get(Con, [
                {isdeleted, false},
                {'or', [
                    {id,    proplists:get_value(owner_id,   Params)},
                    {nick,  proplists:get_value(owner_nick, Params)}
                ]},
                {fields, [
                    id,
                    money
                ]},
                {limit, 1}
            ])
        } of
            {   {ok, [{Cdoclotpl}]},
                {ok, [{Userpl}]}
            } ->
                Cdoclot_owner_id    = proplists:get_value(owner_id, Cdoclotpl),
                Cdoc_id             = proplists:get_value(cdoc_id,  Cdoclotpl),
                Betmin              = proplists:get_value(betmin,   Cdoclotpl),
                Betmax              = proplists:get_value(betmax,   Cdoclotpl),
                Dtstart             = proplists:get_value(dtstart,  Cdoclotpl),
                Dtstop              = proplists:get_value(dtstop,   Cdoclotpl),
                Money               = proplists:get_value(money,    Userpl),
                Newmoney            = Money - Price,
                %%
                %% Вычисляем, кто до этого, сделал ставку.
                %%
                Mbmaxprev = empdb_dao_cdocbet:get(Con, [
                    {isdeleted, false},
                    {price, {lt, Price + ?EMPDB_BIZ_CDOCBET_EPSILON}},
                    {cdoclot_id, Cdoclot_id},
                    {limit, 1},
                    {order, [
                        {desc, price}
                    ]}
                ]),
                %%
                %% Вычисляем минимально возможную цену ставки.
                %% Она должна быть больше и равна минимальной ставки за лот,
                %% и больше предцыдущей ставки
                %%
                Betminc =
                    case Mbmaxprev of
                        {ok, [{Maxprev1}]} ->
                            proplists:get_value(price, Maxprev1)
                            + ?EMPDB_BIZ_CDOCBET_EPSILON;
                        _ ->
                            Betmin
                    end,
                case (
                    (
                        Cdocbet_owner_id =/= Cdoclot_owner_id
                    ) and (
                        Price =< Money
                    ) and (
                        (Betminc    =< Price) and (Price    =<  Betmax)
                    ) and (
                        (Dtstart    =< Now  ) and (Now      =<  Dtstop)
                    )
                ) of
                    true ->
                        case Mbmaxprev of
                            {ok, [{Maxprev}]} ->
                                Maxprev_owner_id    =
                                    proplists:get_value(owner_id, Maxprev),
                                Maxprev_price       =
                                    proplists:get_value(price, Maxprev),
                                %%
                                %% Возвращаем деньги пользователю.
                                %%
                                {ok, _} = empdb_dao_pers:update(Con,[
                                    {id,    Maxprev_owner_id},
                                    {money, {incr, Maxprev_price}}
                                ]),
                                %% Шлем сообщение, что ставка бита
                                %% Тому кто ставил
                                {ok, _} = empdb_dao_event:create(Con, [
                                    {eventobj_alias,    cdocbet},
                                    {eventact_alias,    delete},
                                    {owner_id,          Maxprev_owner_id},
                                    {target_id,         proplists:get_value(id, Maxprev)},
                                    {pers_id,           Cdoclot_owner_id},
                                    {eventtype_alias,   delete_cdocbet_beatrate}
                                ]),
                                %% Шлем сообщение, что ставка бита
                                %% Владельцу аукциона
                                {ok, _} = empdb_dao_event:create(Con, [
                                    {eventobj_alias,    cdocbet},
                                    {eventact_alias,    delete},
                                    {owner_id,          Cdoclot_owner_id},
                                    {target_id,         proplists:get_value(id, Maxprev)},
                                    {pers_id,           Cdocbet_owner_id},
                                    {eventtype_alias,   delete_cdocbet_beatrate}
                                ]),
                                {ok, _} = empdb_dao_pay:create(Con, [
                                    {pers_id,           Maxprev_owner_id},
                                    {paytype_alias,     cdocbet_in},
                                    {isincome,          true},
                                    {price,             Maxprev_price}
                                ]),
                                ok;
                            Some ->
                                ok
                        end,
                        %%
                        %% Списываем деньги у участника аукциона.
                        %% Если он станет победителем,
                        %% то это будет плата за товар.
                        %% Если кто-то сделает большую ставку,
                        %% то эти деньги вернем на следующей итерации.
                        %%
                        {ok, _} = empdb_dao_pers:update(Con,[
                            {id,    proplists:get_value(id,   Userpl)},
                            {money, {decr, Price}}
                        ]),
                        {ok, _} = empdb_dao_pay:create(Con, [
                            {pers_id,           proplists:get_value(id,   Userpl)},
                            {paytype_alias,     cdocbet_out},
                            {isincome,          false},
                            {price,             Price}
                        ]),
                        {ok, [{Cdocbet}]} = empdb_dao_cdocbet:create(Con,[
                            {filter, [
                                id
                            ]}
                            |Params
                        ]),
                        case Price =:= Betmax of
                            true ->
                                %%
                                %% Назначена цена выкупа.
                                %% Человек автоматически становится победителем.
                                %%
                                %% Старому владельцу зачисляются деньги.
                                %%
                                {ok, _} = empdb_dao_pay:create(Con, [
                                    {pers_id,           cdoclot_owner_id},
                                    {paytype_alias,     cdoclot_in},
                                    {isincome,          true},
                                    {price,             Price}
                                ]),
                                %%
                                %% Меняется владельца страны.
                                %%
                                {ok, [{Cdocpl}]} =
                                    empdb_dao_cdoc:update(Con, [
                                        {id,                Cdoc_id},
                                        {cdoclot_id,        null},
                                        {cdoclot_betmin,    null},
                                        {cdoclot_betmax,    null},
                                        {cdoclot_dtstart,   null},
                                        {cdoclot_dtstop,    null},
                                        {cdocbet_id,        null},
                                        {cdocbet_owner_id,  null},
                                        {cdocbet_owner_nick,null},
                                        {cdocbet_price,     null},
                                        {owner_id,          Cdocbet_owner_id}
                                    ]),
                                %%
                                %%  Меняется владельца страны.
                                %%  Отбираем страну у старого владельца.
                                %%
                                {ok, _} = empdb_dao_pers:update(Con, [
                                    {id,            Cdoclot_owner_id},
                                    {own_cdoc_id,   null},
                                    {money,         {incr, Price}}
                                ]),
                                %%
                                %%  Меняется владельца страны.
                                %%  ОТдаем страну новому владельцу.
                                %%
                                {ok, _} = empdb_dao_pers:update(Con, [
                                    {id,            Cdocbet_owner_id},
                                    {own_cdoc_id,
                                        proplists:get_value(id, Cdocpl)},
                                    {citizen_cdoc_id,
                                        proplists:get_value(id, Cdocpl)}
                                ]),
                                %% Победителю шлем сообщение, что он победил
                                {ok, _} = empdb_dao_event:create(Con, [
                                    {eventobj_alias,    cdocbet},
                                    {eventact_alias,    create},
                                    {owner_id,          Cdocbet_owner_id},
                                    {target_id,         proplists:get_value(id, cdocbet)},
                                    {pers_id,           Cdoclot_owner_id},
                                    {eventtype_alias,   create_cdocbet_win}
                                ]),
                                %% Владельцу аукциона шлем сообщение,
                                %% что аукцион окончен
                                {ok, _} = empdb_dao_event:create(Con, [
                                    {eventobj_alias,    cdoclot},
                                    {eventact_alias,    delete},
                                    {owner_id,          Cdoclot_owner_id},
                                    {doc_id,            Cdoclot_id},
                                    {pers_id,           Cdocbet_owner_id},
                                    {eventtype_alias,   delete_cdoclot_win}
                                ]),
                                {ok, _} = empdb_dao_cdoclot:update(Con,[
                                    {filter, [
                                        {isdeleted, false},
                                        {id, Cdoclot_id}
                                    ]},
                                    {values, [
                                        {isdeleted, true}
                                    ]}
                                ]),
                                 {ok, [{Cdocbet}]};
                            false ->
                                %%
                                %% Штатная ситуация.
                                %% Человек (пока) не победил.
                                %%
                                 {ok, [{Cdocbet}]}
                        end;
                    _ ->
                        {error, {something_wrong, {[
                            {'now',             Now},
                            {cdocbet_owner_id,  Cdocbet_owner_id},
                            {cdoclot_owner_id,  Cdoclot_owner_id},
                            {money,             Money},
                            {price,             Price},
                            {betmin,            Betmin},
                            {betmax,            Betmax},
                            {dtstart,           Dtstart},
                            {dtstop,            Dtstop}
                        ]}}}
                end;
            {{ok, []}, {ok, _}} ->
                {error, no_such_cdoclot};
            {{ok, _}, {ok, []}} ->
                {error, no_such_pers};
            {Error1, {ok, _}} ->
                Error1;
            {{ok, _}, Error2} ->
                Error2
        end
    end).


update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_cdocbet:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_cdocbet:get(
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
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_cdocbet:get(
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

is_blog_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_cdocbet:is_owner(Con, Uid, Oid)
    end).
