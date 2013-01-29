%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_exile).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").


-define(DELETE_EXILE_OTHER, 2.0).
-define(DELETE_EXILE_SELF,  3.0).


%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Блоги
%%
-export([
    count/1,
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
%% Блоги
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Params)->
    empdb_dao:with_transaction(fun(Con)->
        case empdb_dao_exile:get(Con, [{isdeleted, false}|Params]) of
            {ok, []} ->
                empdb_dao_exile:create(Con, Params);
            {ok, _} ->
                {error, not_uniq_exile};
            Else ->
                Else
        end
    end).

update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_exile:update(Con, Params)
    end).

count(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_exile:count(Con, [{isdeleted, false}|Params])
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_exile:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_exile:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        Exilelist  = empdb_dao_exile:get(Con, [{isdeleted, false}|Params], []),
        Savior_id = proplists:get_value(self@pers_id, Params),
        delete_by_pers_id(Con, Exilelist, Savior_id)
    end).


delete_by_pers_id(Con, {ok, []}, _)->
    {ok, []};

delete_by_pers_id(Con, {ok, [{Params}]}, Savior_id)->
    Id          = proplists:get_value(id, Params),
    Pers_id     = proplists:get_value(pers_id, Params),
    Pers_nick   = proplists:get_value(pers_nick, Params),

    {ok, [{Mbsaviorpl}]} =
        empdb_dao_pers:get(
            Con,
            [
                {id,    Savior_id},
                {fields, [
                    id,
                    money
                ]},
                {limit, 1}
            ]
        ),

    {ok, [{Mbperspl}]} =
        empdb_dao_pers:get(
            Con,
            [
                {'or', [
                    {id,    Pers_id},
                    {nick,  Pers_nick}
                ]},
                {fields, [
                    id,
                    nick
                ]},
                {limit, 1}
            ]
        ),
        
    Price =
        case Pers_id == Savior_id of
            true ->
                ?DELETE_EXILE_SELF;
            _    ->
                ?DELETE_EXILE_OTHER
        end,
        
    Money = proplists:get_value(money, Mbsaviorpl),

    case {Price =< Money} of
        {true} ->
            %% Создаем запись в лог кошелька пользователя
            {ok, _} =
                empdb_dao_pay:create(Con, [
                    {pers_id,           Savior_id},
                    {paytype_alias,     exile_delete},
                    {isincome,          false},
                    {info,              [
                        <<"for pers ">>,
                            empdb_convert:to_binary(
                                proplists:get_value(id, Mbperspl)
                            ),
                        <<" (">>,
                            empdb_convert:to_binary(
                                proplists:get_value(nick, Mbperspl)
                            ),
                        <<")">>
                    ]},
                    {price,             Price}
                ]),
            %% Снимаем с пользователя деньги
            {ok, _} =
                empdb_dao_pers:update(Con,[
                    {id,    Savior_id},
                    {money, {decr, Price}}
                ]),
            {ok, [{Respl}]} =
                empdb_dao_exile:update(Con, [
                    {filter, [
                        {isdeleted, false},
                        {id, Id}
                    ]},
                    {values, [
                        {isdeleted, true},
                        {savior_id, Savior_id}
                    ]}
                ]),
            {ok, [{[
                {price, Price},
                {money, Money - Price}
                |Respl
            ]}]} ;
        {false} ->
            {error, {not_enough_money, {[
                {money, Money},
                {price, Price}
            ]}}}
    end.


% remove_expired()

is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_exile:is_owner(Con, Uid, Oid)
    end).
