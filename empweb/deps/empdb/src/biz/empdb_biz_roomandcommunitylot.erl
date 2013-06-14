%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_roomandcommunitylot).

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
    get/2
]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          ЗНАЧИМЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Блоги
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(Aparams)->
    proplists:get_value(limit, Aparams),


    Params =
        proplists:delete(
            limit,
            proplists:delete(
                offset,
                Aparams
            )
        ),

    empdb_dao:with_connection(fun(Con)->
        case {
            empdb_dao_roomlot:get(
                Con,
                [
                    {isdeleted, false}
                    |Params
                ] ++ [
                    {order, {desc, created}}
                ]
            ),
            empdb_dao_communitylot:get(
                Con,
                [
                    {isdeleted, false}
                    |Params
                ] ++ [
                    {order, {desc, created}}
                ]
            )
        } of
            {
                {ok, Roomlotlist},
                {ok, Communitylotlist}
            } ->
                Fulllist =
                    lists:append([
                        Roomlotlist,
                        Communitylotlist
                    ]),
                {ok,
                    lists:sort(
                        fun({I1pl}, {I2pl})->
                            I1created = proplists:get_value(created, I1pl),
                            I2created = proplists:get_value(created, I2pl),
                            (I1created =< I2created)
                        end,
                        Fulllist
                    )
                };
            {{ok, _}, Communityloterror} ->
                Communityloterror;
            {Roomlotlist, _} ->
                Roomlotlist
        end
    end).

get(Params, Fileds)->
    ?MODULE:get(Params, Fileds).



nowsec() ->
    {Mgs,Sec, _mis} = erlang:now(),
    Now = Mgs * 1000000 + Sec,
    Now.
