%%
%% @file    empweb_biz_conf.erl
%%          Бизнес логика по работе с системными настройками,
%%          языками, и связанными с ними объектами.
%%
%%          Зависит от модуля empdb_biz_lang. Все внешние функуции принимают
%%              proplist()
%%          и возвращают:
%%              {ok, [Obj::{proplist()}]}
%%              |   {error, Reason::atom()}
%%              |   {error, {Reason::atom(), Info::atom()}}
%%              |   {error, {Reason::atom(), Info::[Obj::{proplist()}]}}
%%
-module(empweb_biz_conf).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Списки языков
%%
-export([
    update_tr/1,
    create_tr/1,
    get_tr/1,
    get_tr/2
]).

%%
%% Списки переводов 
%%
-export([
    update_lang/1,
    create_lang/1,
    get_lang/1,
    get_lang/2
]).

%% ==========================================================================
%% Внешние функции
%% ==========================================================================

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Списки языков
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_lang(Params)->
    empdb_biz_lang:create_lang(Params).

update_lang(Params)->
    empdb_biz_lang:update_lang(Params).

get_lang(Params)->
    empdb_biz_lang:get_lang(Params).

get_lang(Params, Fields)->
    empdb_biz_lang:get_lang(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Списки переводов
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_tr(Params)->
    empdb_biz_lang:create_tr(Params).

update_tr(Params)->
    empdb_biz_lang:update_tr(Params).

get_tr(Params)->
    empdb_biz_lang:get_tr(Params).

get_tr(Params, Fields)->
    empdb_biz_lang:get_tr(Params, Fields).

