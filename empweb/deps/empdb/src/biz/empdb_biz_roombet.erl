%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_roombet).

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
        Roomlot_id  = proplists:get_value(roomlot_id, Params),
        Roombet_owner_id   = proplists:get_value(owner_id, Params),
        Price              = proplists:get_value(price, Params, 0),
        Now                = nowsec(),
        case {
            %%
            %% Выясним информацию о покупателе и о лоте.
            %%
            empdb_dao_roomlot:get(Con, [
                {isdeleted, false},
                {id, Roomlot_id},
                {fields, [
                    room_id,
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
            {   {ok, [{Roomlotpl}]},
                {ok, [{Userpl}]}
            } ->
                Roomlot_owner_id    = proplists:get_value(owner_id, Roomlotpl),
                Room_id             = proplists:get_value(room_id,  Roomlotpl),
                Betmin              = proplists:get_value(betmin,   Roomlotpl),
                Betmax              = proplists:get_value(betmax,   Roomlotpl),
                Dtstart             = proplists:get_value(dtstart,  Roomlotpl),
                Dtstop              = proplists:get_value(dtstop,   Roomlotpl),
                Money               = proplists:get_value(money,    Userpl),
                Newmoney            = Money - Price,
                %%
                %% Вычисляем, кто до этого, сделал ставку.
                %%
                Mbmaxprev = empdb_dao_roombet:get(Con, [
                    {isdeleted, false},
                    {price, {lt, Price + ?EMPDB_BIZ_ROOMBET_EPSILON}},
                    {roomlot_id, Roomlot_id},
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
                            + ?EMPDB_BIZ_ROOMBET_EPSILON;
                        _ ->
                            Betmin
                    end,
                case (
                    (
                        Roombet_owner_id =/= Roomlot_owner_id
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
                                    {eventobj_alias,    roombet},
                                    {eventact_alias,    delete},
                                    {owner_id,          Maxprev_owner_id},
                                    {target_id,         proplists:get_value(id, Maxprev)},
                                    {pers_id,           Roomlot_owner_id},
                                    {eventtype_alias,   delete_roombet_beatrate}
                                ]),
                                %% Шлем сообщение, что ставка бита
                                %% Владельцу аукциона
                                {ok, _} = empdb_dao_event:create(Con, [
                                    {eventobj_alias,    roombet},
                                    {eventact_alias,    delete},
                                    {owner_id,          Roomlot_owner_id},
                                    {target_id,         proplists:get_value(id, Maxprev)},
                                    {pers_id,           Roombet_owner_id},
                                    {eventtype_alias,   delete_roombet_beatrate}
                                ]),
                                {ok, _} = empdb_dao_pay:create(Con, [
                                    {pers_id,           Maxprev_owner_id},
                                    {paytype_alias,     roombet_in},
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
                            {paytype_alias,     roombet_out},
                            {isincome,          false},
                            {price,             Price}
                        ]),
                        Roombet = empdb_dao_roombet:create(Con,[
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
                                %% 1) Старому владельцу зачисляются деньги.
                                %% 2) Меняется владельца страны.
                                %% 
                                {ok, _} = empdb_dao_pay:create(Con, [
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
                                        {owner_id,          Roombet_owner_id}
                                    ]),
                                {ok, _} = empdb_dao_pers:update(Con, [
                                    {id,        Roomlot_owner_id},
                                    {own_room_id,
                                        proplists:get_value(id, Roompl)},
                                    {citizen_room_id,
                                        proplists:get_value(id, Roompl)},
                                    {money,     {incr, Price}}
                                ]),
                                %% Победителю шлем сообщение, что он победил
                                {ok, _} = empdb_dao_event:create(Con, [
                                    {eventobj_alias,    roombet},
                                    {eventact_alias,    create},
                                    {owner_id,          Roombet_owner_id},
                                    {target_id,         proplists:get_value(id, Roombet)},
                                    {pers_id,           Roomlot_owner_id},
                                    {eventtype_alias,   create_roombet_win}
                                ]),
                                %% Владельцу аукциона шлем сообщение,
                                %% что аукцион окончен
                                {ok, _} = empdb_dao_event:create(Con, [
                                    {eventobj_alias,    roomlot},
                                    {eventact_alias,    delete},
                                    {owner_id,          Roomlot_owner_id},
                                    {doc_id,            Roomlot_id},
                                    {pers_id,           Roombet_owner_id},
                                    {eventtype_alias,   delete_roomlot_win}
                                ]),
                                {ok, _} = empdb_dao_roomlot:update(Con,[
                                    {filter, [
                                        {isdeleted, false},
                                        {id, Roomlot_id}
                                    ]},
                                    {values, [
                                        {isdeleted, true}
                                    ]}
                                ]),
                                Roombet;
                            false -> 
                                %%
                                %% Штатная ситуация. 
                                %% Человек (пока) не победил.
                                %%
                                Roombet
                        end;
                    _ ->
                        {error, {something_wrong, {[
                            {'now',             Now},
                            {roombet_owner_id,  Roombet_owner_id},
                            {roomlot_owner_id,  Roomlot_owner_id},
                            {money,             Money},
                            {price,             Price},
                            {betmin,            Betmin},
                            {betmax,            Betmax},
                            {dtstart,           Dtstart},
                            {dtstop,            Dtstop}
                        ]}}}
                end;
            {{ok, []}, {ok, _}} ->
                {error, no_such_roomlot};
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
        empdb_dao_roombet:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roombet:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roombet:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_blog_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roombet:is_owner(Con, Uid, Oid)
    end).
