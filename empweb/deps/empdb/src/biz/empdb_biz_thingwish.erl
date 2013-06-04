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
            {ok, Results} ->
                Nresults =
                    lists:map(
                        fun({Result1})->
                            Count1 = proplists:get_value(count, Result1),
                            %Thingtype_alias = proplists:get_value(thingtype_alias, Result),
                            Thingtype_parent_alias1 = proplists:get_value(thingtype_parent_alias, Result1),

                            %Thingtype_id = proplists:get_value(thingtype_id, Result),
                            Thingtype_parent_id1 = proplists:get_value(thingtype_parent_id, Result1),

                            All =
                                lists:foldl(
                                    fun({Result2}, Acc)->
                                        case Thingtype_parent_id1 == proplists:get_value(thingtype_alias, Result2) of
                                            true ->
                                                proplists:get_value(count, Result2) + Acc;
                                            false ->
                                                Acc
                                        end
                                    end,
                                    Count1,
                                    Results
                                ),
                            {[{all, All}|Result1]}
                        end,
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
