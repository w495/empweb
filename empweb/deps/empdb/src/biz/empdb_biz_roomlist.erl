%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_roomlist).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").


% 
% -define\(DELETE_ROOMLIST, 2.0).
% 

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
        case empdb_dao_roomlist:get(Con, [{isdeleted, false}|Params]) of
            {ok, []} ->
                empdb_dao_roomlist:create(Con, Params);
            {ok, _} ->
                {error, not_uniq_roomlist};
            Else ->
                Else
        end
    end).

update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roomlist:update(Con, Params)
    end).

count(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roomlist:count(Con, [{isdeleted, false}|Params])
    end).
    
get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roomlist:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roomlist:get(Con, [{isdeleted, false}|Params], Fileds)
    end).
% 
% delete(Params)->
%     empdb_dao:with_transaction(fun(Con)->
%         case proplists:get_value(pers_id, Params) of
%             undefined ->
%                 empdb_dao_roomlist:update(Con, [
%                     {filter, [
%                         {isdeleted, false}
%                         |Params
%                     ]},
%                     {values, [{isdeleted, true}]}
%                 ]);
%             Pers_id ->
%             
%                 {ok, _} =
%                     empdb_dao_pay:create(Con, [
%                         {pers_id,           Pers_id},
%                         {paytype_alias,     roomlist_delete},
%                         {isincome,          false},
%                         {price,             Price}
%                     ]),
%                 empdb_dao_roomlist:update(Con, [
%                     {filter, [
%                         {isdeleted, false}
%                         |Params
%                     ]},
%                     {values, [{isdeleted, true}]}
%                 ]);
%         end
%     end).
% 

delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        Roomlist = empdb_dao_roomlist:get(Con, [{isdeleted, false}|Params], []),
        Selfpersid = proplists:get_value(self@pers_id, Params),
        delete_by_pers_id(Con, Roomlist, Selfpersid)
    end).


delete_by_pers_id(Con, {ok, []}, _)->
    {ok, []};

delete_by_pers_id(Con, {ok, [{Params}]}, Selfpersid)->
    Id          = proplists:get_value(id, Params),
    Pers_id     = proplists:get_value(pers_id, Params),
    Pers_nick   = proplists:get_value(pers_nick, Params),
    
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
                    money
                ]},
                {limit, 1}
            ]
        ),


    {ok,[{Servicepl}]} =
        empdb_dao_service:get(
            Con,
            [
                {alias, delete_roomlist_price},
                {fields, [price]},
                {limit, 1}
            ]
        ),

    Price = proplists:get_value(price, Servicepl),
    Money = proplists:get_value(money, Mbperspl),
 
    case {Price =< Money, Pers_id == Selfpersid} of
        {true, true} ->
            %% Создаем запись в лог кошелька пользователя
            {ok, _} =
                empdb_dao_pay:create(Con, [
                    {pers_id,           Pers_id},
                    {paytype_alias,     roomlist_delete},
                    {isincome,          false},
                    {price,             Price}
                ]),
            %% Снимаем с пользователя деньги
            {ok, _} =
                empdb_dao_pers:update(Con,[
                    {id,    Pers_id},
                    {money, {decr, Price}}
                ]),
            {ok, [{Respl}]} =
                empdb_dao_roomlist:update(Con, [
                    {filter, [
                        {isdeleted, false},
                        {id, Id}
                    ]},
                    {values, [{isdeleted, true}]}
                ]),
            {ok, [{[
                {price, Price},
                {money, Money - Price}
                |Respl
            ]}]} ;
        {false, true} ->
            {error, {not_enough_money, {[
                {money, Money},
                {price, Price}
            ]}}};
        {_, false} ->
            empdb_dao_roomlist:update(Con, [
                {filter, [
                    {isdeleted, false},
                    {id, Id}
                ]},
                {values, [{isdeleted, true}]}
            ])
    end.
                
is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_roomlist:is_owner(Con, Uid, Oid)
    end).
