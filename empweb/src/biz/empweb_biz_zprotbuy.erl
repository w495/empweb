%%
%% @file    empweb_biz_conf.erl
%%          Бизнес логика по работе с системными настройками,
%%          языками, и связанными с ними объектами.
%%
%%          Зависит от модуля empdb_biz_zprotbuy. Все внешние функуции принимают
%%              proplist()
%%          и возвращают:
%%              {ok, [Obj::{proplist()}]}
%%              |   {error, Reason::atom()}
%%              |   {error, {Reason::atom(), Info::atom()}}
%%              |   {error, {Reason::atom(), Info::[Obj::{proplist()}]}}
%%
-module(empweb_biz_zprotbuy).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Списки языков
%%
-export([
    update/1,
    create/1,
    count/1,
    get/1,
    get/2,
    delete/1
]).


%% ==========================================================================
%% Внешние функции
%% ==========================================================================

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Списки языков
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Params)->
    empdb_biz_zprotbuy:create(Params).

update(Params)->
    empdb_biz_zprotbuy:update(Params).

count(Params)->
    empdb_biz_zprotbuy:count(Params).
    
get(Params)->
    empdb_biz_zprotbuy:get(Params).

get(Params, Fields)->
    empdb_biz_zprotbuy:get(Params, Fields).

delete(Params)->
    empdb_biz_zprotbuy:delete(Params).

    