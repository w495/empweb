%% Author: w-495
%% Created: 25.07.2012
-module(empdb_biz_experwish).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").

%
% -define\(EXPER_COEF,  0.5).
%

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

create(Params)->
    empdb_dao:with_transaction(fun(Con)->
        %%
        %% Берем получателя, и смотрим сколько у опыта
        %% и недостатка опыта
        %%
        {ok, [{Mbownerpl}]} =
            empdb_dao_pers:get(Con, [
                {'or', [
                    {id,
                        proplists:get_value(
                            owner_id,
                            Params,
                            proplists:get_value(owner_id,   Params)
                        )
                    },
                    {nick,
                        proplists:get_value(
                            owner_nick,
                            Params,
                            proplists:get_value(owner_nick, Params)
                        )
                    }
                ]},
                {fields, [
                    id,
                    experlack
                ]},
                {limit, 1}
            ]),
        %%
        %% Смотрим какой недостаток опыта имеет получатель,
        %% для перехода на следующий уровень.
        %%
        Experlack =
            case proplists:get_value(experlack,   Mbownerpl,    0) of
                null ->
                    0;
                Val  ->
                    Val
            end,

        Exper = proplists:get_value(exper,   Params,    Experlack),

        Price = exper2price(Con, Exper),

        case Exper of
            0 ->
                {error, {wrong_exper, {[
                    {exper, Exper},
                    {price, Price}
                ]}}};
            _ ->
                case empdb_dao_experwish:create(Con,[
                    {price, Price}
                    |Params
                ]) of
                    {ok, [{Respl}]} ->
                        %%
                        %% Изменим состояние получателя.
                        %%
                        {ok, [{Newownerpl}]} =
                            empdb_dao_pers:update(Con,[
                                {id,    proplists:get_value(id,   Mbownerpl)},
                                {experwish,
                                    {
                                        case Exper > 0 of
                                            true ->
                                                incr;
                                            false ->
                                                decr
                                        end,
                                        erlang:abs(Exper)
                                    }
                                },
                                {fields, [
                                    exper,
                                    experlack,
                                    experwish,
                                    experlackprice,
                                    authority_id,
                                    authority_alias
                                ]}
                            ]),
                        {ok, [
                            {[
                                {authority_alias,
                                    proplists:get_value(
                                        authority_alias,
                                        Newownerpl
                                    )
                                },
                                {authority_id,
                                    proplists:get_value(
                                        authority_id,
                                        Newownerpl
                                    )
                                },
                                {experlackprice,
                                    proplists:get_value(
                                        experlackprice,
                                        Newownerpl
                                    )
                                },
                                {experlack,
                                    proplists:get_value(
                                        experlack,
                                        Newownerpl
                                    )
                                },
                                {experwish,
                                    proplists:get_value(
                                        experwish,
                                        Newownerpl
                                    )
                                },
                                {exper,
                                    proplists:get_value(
                                        exper,
                                        Newownerpl
                                    )
                                },
                                {price, Price}
                                |Respl
                            ]}
                        ]};
                    Else ->
                        Else
                end;
            {_, false} ->
                {error, {not_enough_money, {[
                    {exper, Exper},
                    {price, Price}
                ]}}}
        end
    end).

exper2price(Con, Exper) ->
    {ok,[{Servicepl}]} =
        empdb_dao_service:get(
            Con,
            [
                {alias, create_experbuy_coef},
                {fields, [price]},
                {limit, 1}
            ]
        ),
    Price = proplists:get_value(price, Servicepl),
    erlang:abs(Price * Exper).

update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_experwish:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_experwish:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_experwish:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_experwish:is_owner(Con, Uid, Oid)
    end).
