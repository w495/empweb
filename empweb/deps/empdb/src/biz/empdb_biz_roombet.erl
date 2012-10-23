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

now() ->
    {Mgs,Sec, _mis} = erlang:now(),
    Now = Mgs * 1000000 + Sec,
    Now.

create(Params)->
    empdb_dao:with_connection(fun(Con)->
%         Roomlot_id  = proplists:get_value(roomlot_id, Params),
%         Owner_id    = proplists:get_value(owner_id, Params),
% 
%         Price       = proplists:get_value(price, Params, 0),
%         Now         = now(),
% 
%         case {
%             empdb_dao_roomlot:get(Con, [
%                 {isdeleted, false},
%                 {id, Roomlot_id},
%                 {fields, [
%                     dtstart,
%                     dtstop,
%                     betmin,
%                     betmax
%                 ]},
%                 {limit, 1}
%             ]),
%             empdb_dao_pers:get(Con, [
%                 {'or', [
%                     {id,    proplists:get_value(buyer_id,   Params)},
%                     {nick,  proplists:get_value(buyer_nick, Params)}
%                 ]},
%                 {fields, [
%                     id,
%                     money
%                 ]},
%                 {limit, 1}
%             ]),
%         } of
%             {   {ok, [{Roomlotpl}]},
%                 {ok, [{Userpl}]}
%             } ->
%                 Betmin      = proplists:get_value(betmin,   Roomlotpl),
%                 Betmax      = proplists:get_value(betmax,   Roomlotpl),
%                 Dtstart     = proplists:get_value(dtstart,  Roomlotpl),
%                 Dtstop      = proplists:get_value(dtstop,   Roomlotpl),
%                 Money       = proplists:get_value(money,    Userpl),
%                 Newmoney    = Money - Price,
%                 case (
%                     (
%                         Price =< Money
%                     ) and (
%                         (Betmin     =< Price) and (Price    =<  Betmax)
%                     ) and (
%                         (Dtstart    =< Now  ) and (Now      =<  Dtstop)
%                     )
%                 ) of
%                     true ->
%                         X = empdb_dao_pers:update(Con,[
%                             {id,    proplists:get_value(id,   Userpl)},
%                             {money, Newmoney}
%                         ]),

                empdb_dao_roombet:create(Con, Params)
%             Error ->
%                 Error;
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roombet:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roombet:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roombet:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_blog_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_roombet:is_owner(Con, Uid, Oid)
    end).
