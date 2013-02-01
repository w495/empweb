%%
%%
-module(empweb_biz_invisbuy).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Списки языков
%%
-export([
    count/1,
    update/1,
    create/1,
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
    empdb_biz_invisbuy:create(Params).

update(Params)->
    empdb_biz_invisbuy:update(Params).

count(Params)->
    empdb_biz_invisbuy:count(Params).

get(Params)->
    empdb_biz_invisbuy:get(Params).

get(Params, Fields)->
    empdb_biz_invisbuy:get(Params, Fields).

delete(Params)->
    empdb_biz_invisbuy:delete(Params).


    