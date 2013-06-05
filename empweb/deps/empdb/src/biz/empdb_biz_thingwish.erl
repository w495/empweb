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


count_by_thingtype(Params)->
    empdb_dao:with_transaction(fun(Con)->
        case empdb_dao_thingwish:count_by_thingtype(Con, [{isdeleted, false}|Params]) of
            {ok, Results_} ->
                %io:format("List = ~p ", [Results]),
                Results =
                    [
                        {[{all,0},{alias,<<"all">>},{parent_id,null},{id,1},{count,5}]},
                        {[{all,0},{alias,<<"counter">>},{parent_id,1},{id,2},{count,10}]},
                        {[{all,0},{alias,<<"closes">>},{parent_id,1},{id,3},{count,20}]}
                    ],

                Nresults =
                    lists:foldr(
                        fun({Result1}, Acclist)->
                            Id1 =           proplists:get_value(id,         Result1),
                            Parent_id1 =    proplists:get_value(parent_id,  Result1),
                            Count1 =        proplists:get_value(count,  Result1),
                            All1 =          proplists:get_value(all,  Result1),

                            {Sum, List} =
                                lists:foldr(
                                    fun({Result2}, {Sum2, List2})->
                                        Id2 =           proplists:get_value(id,         Result2),
                                        Parent_id2 =    proplists:get_value(parent_id,  Result2),
                                        Count2 =        proplists:get_value(count,      Result2),
                                        All2 =          proplists:get_value(all,        Result2),

                                        io:format("~n~n~n~n", []),
                                        io:format("x Id1 =        ~p ~n", [Id1]),
                                        io:format("x Parent_id1 = ~p ~n", [Parent_id1]),
                                        io:format("x Count1 =     ~p ~n", [Count1]),
                                        io:format("x Id2 =        ~p ~n", [Id2]),
                                        io:format("x Parent_id2 = ~p ~n", [Parent_id2]),
                                        io:format("x Count2 =     ~p ~n", [Count2]),
                                        io:format("x Sum2 =       ~p ~n", [Sum2]),


                                        io:format("x ~n~n~n~n", []),

                                        case Parent_id1 of
                                            Id2 ->
                                                {Sum2 + Count2, List2};
                                            _ ->
                                                {Sum2, List2}
                                        end
                                    end,
                                    %fun({Result2}, {Sum2, List2})->
                                        %case PThingtype_id1 == proplists:get_value(id, Result2) of
                                            %true ->
                                                %Nsum =
                                                   %% proplists:get_value(all, Result1, 0) +
                                                    %proplists:get_value(count, Result2) +
                                                    %Sum2,
                                                %io:format(" ~n~n~n Nsum = ~p ~p ~p ~n~n~n", [Nsum, Thingtype_id1, proplists:get_value(parent_id, Result2)]),
                                                %{   Nsum,
                                                    %[{[{all,Nsum}|proplists:delete(all, Result2)]}|List2]
                                                %};
                                            %false ->
                                            %io:format(" ~n~n~n Nsum = x0 ~p ~p ~n~n~n", [Thingtype_id1, proplists:get_value(parent_id, Result2)]),
                                                %{Sum2, [{[{all,Sum2}|proplists:delete(all, Result2)]}|List2]}
                                        %end
                                    %end,
                                    {Count1, Results},
                                    %[{Result1}|Acclist]
                                    Acclist
                                ),
                            List
                        end,
                        Results,
                        Results
                    ),
                {ok, Nresults};
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
