%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_thingwish).

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
    count/1,
    count_by_thingtype/1,
    get/1,
    get/2,
    create/1,
    update/1,
    delete/1
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

        case empdb_dao_thingwish:get(Con, [
            {'and', [
                {'or', [
                    {owner_id, proplists:get_value(owner_id, Params)},
                    {owner_nick, proplists:get_value(owner_nick, Params)}
                ]},
                {'or', [
                    {thing_id, proplists:get_value(thing_id, Params)},
                    {thing_alias, proplists:get_value(thing_alias, Params)}
                ]},
                {isdeleted, false}
            ]}
        ]) of
            {ok, []} ->
                empdb_dao_thingwish:create(Con, Params);
            {ok, Ok} ->
                {error, dublicate_thing};
            Else ->
                Else
        end
    end).

update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingwish:update(Con, Params)
    end).

delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingwish:update(Con, [
            {filter, [
                {isdeleted, false}
                |Params
            ]},
            {values, [
                {isdeleted, true}
            ]}
        ])
    end).


wfoldr(F, Accu) ->
    foldr(F, Accu, 1, Accu).
foldr(F, Accu, _N, []) ->
    Accu;
foldr(F, Accu, N, [Hd|Tail]) ->
    Accx = foldr(F, Accu, N + 1, Tail),
    [HAccx|TAccx] = lists:reverse(Accx),
    [HAccx|F(HAccx, lists:reverse(TAccx))].

count_by_thingtype(Params)->
    empdb_dao:with_transaction(fun(Con)->
        case empdb_dao_thingwish:count_by_thingtype(Con, [{isdeleted, false}|Params]) of
            {ok, Results} ->
                %io:format("List = ~p ", [Results]),
                %Results =
                    %[
                        %{[{alias,<<"all">>},{parent_id,null},{id,1},{count,0}]},
                        %{[{alias,<<"counter">>},{parent_id,1},{id,2},{count,10}]},
                        %{[{alias,<<"counterx">>},{parent_id,2},{id,3},{count,5}]},
                        %{[{alias,<<"closes">>},{parent_id,1},{id,100},{count,20}]},
                        %{[{alias,<<"counterx">>},{parent_id,2},{id,200},{count,5}]}
                    %],
                Nresults =
                    wfoldr(
                        fun({Result1}, Acclist)->
                            Id1 =           proplists:get_value(id,         Result1),
                            Parent_id1 =    proplists:get_value(parent_id,  Result1),
                            Count1 =        proplists:get_value(count,      Result1),
                            All1 =          proplists:get_value(all,        Result1, Count1),

                            lists:foldr(
                                fun({Result2}, List2)->
                                    Id2 =           proplists:get_value(id,         Result2),
                                    Parent_id2 =    proplists:get_value(parent_id,  Result2),
                                    Count2 =        proplists:get_value(count,      Result2),
                                    All2 =          proplists:get_value(all,        Result2,  Count2),
                                    Nall =
                                        case Parent_id1 of
                                            Id2 ->
                                                All1 + All2;
                                            _ ->
                                                All2
                                        end,
                                    [{[{all, Nall}|proplists:delete(all, Result2)]}|List2]
                                end,
                                [],
                                Acclist
                            )
                        end,
                        Results
                    ),
                case empdb_dao_pers:get(Con, [
                    {'or', [
                        {id,    proplists:get_value(owner_id,   Params, null)},
                        {nick,  proplists:get_value(owner_id,   Params, null)}
                    ]},
                    {limit, 1},
                    {fields, [
                        experwish,
                        moneywish
                    ]}

                ]) of
                    {ok, [{Perspl}]} ->
                        {ok, [
                            {[
                                {all, null},
                                {alias, experwish},
                                {parent_id, null},
                                {id, null},
                                {count, proplists:get_value(experwish,   Perspl, null)}
                            ]},
                            {[
                                {all, null},
                                {alias, moneywish},
                                {parent_id, null},
                                {id, null},
                                {count, proplists:get_value(moneywish,   Perspl, null)}
                            ]}
                            |Nresults
                        ]};
                    {ok, []} ->
                        {error, no_such_owner};
                    Else1 ->
                        Else1
                end;
            Else ->
                Else
        end
    end).

count(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingwish:count(Con, [{isdeleted, false}|Params])
    end).


get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingwish:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingwish:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_thingwish:is_owner(Con, Uid, Oid)
    end).
