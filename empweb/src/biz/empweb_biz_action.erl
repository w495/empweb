%%
%% @file    empweb_biz_conf.erl
%%          Бизнес логика по работе с системными настройками,
%%          языками, и связанными с ними объектами.
%%
%%          Зависит от модуля empdb_biz_action. Все внешние функуции принимают
%%              proplist()
%%          и возвращают:
%%              {ok, [Obj::{proplist()}]}
%%              |   {error, Reason::atom()}
%%              |   {error, {Reason::atom(), Info::atom()}}
%%              |   {error, {Reason::atom(), Info::[Obj::{proplist()}]}}
%%
-module(empweb_biz_action).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Списки языков
%%
-export([
    update/1,
    create/1,
    get/1,
    get/2
]).


%% ==========================================================================
%% Внешние функции
%% ==========================================================================

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Списки языков
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Params)->
    empdb_biz_action:create(Params).

update(Params)->
    empdb_biz_action:update(Params).

get(Params)->
    empdb_biz_action:get(Params).

get(Params, Fields)->
    empdb_biz_action:get(Params, Fields).
