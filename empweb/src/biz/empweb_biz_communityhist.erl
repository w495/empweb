%%
%% @file    empweb_biz_conf.erl
%%          Бизнес логика по работе с системными настройками,
%%          языками, и связанными с ними объектами.
%%
%%          Зависит от модуля empdb_biz_communityhist. Все внешние функуции принимают
%%              proplist()
%%          и возвращают:
%%              {ok, [Obj::{proplist()}]}
%%              |   {error, Reason::atom()}
%%              |   {error, {Reason::atom(), Info::atom()}}
%%              |   {error, {Reason::atom(), Info::[Obj::{proplist()}]}}
%%
-module(empweb_biz_communityhist).

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
    get/2
]).


%% ==========================================================================
%% Внешние функции
%% ==========================================================================

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Списки языков
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Params)->
    empdb_biz_communityhist:create(Params).

update(Params)->
    empdb_biz_communityhist:update(Params).


count(Params)->
    empdb_biz_communityhist:count(Params).

get(Params)->
    empdb_biz_communityhist:get(Params).

get(Params, Fields)->
    empdb_biz_communityhist:get(Params, Fields).
