%%
%%
-module(empweb_biz_thingwish).

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
    count_by_thingtype/1,
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
    empdb_biz_thingwish:create(Params).

update(Params)->
    empdb_biz_thingwish:update(Params).

delete(Params)->
    empdb_biz_thingwish:delete(Params).

count(Params)->
    empdb_biz_thingwish:count(Params).

count_by_thingtype(Params) ->
    empdb_biz_thingwish:count_by_thingtype(Params).

get(Params)->
    empdb_biz_thingwish:get(Params).

get(Params, Fields)->
    empdb_biz_thingwish:get(Params, Fields).
