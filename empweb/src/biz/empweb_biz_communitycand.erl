%%
%%
-module(empweb_biz_communitycand).

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
    empdb_biz_communitycand:create(Params).

update(Params)->
    empdb_biz_communitycand:update(Params).

delete(Params)->
    empdb_biz_communitycand:delete(Params).

get(Params)->
    empdb_biz_communitycand:get(Params).

get(Params, Fields)->
    empdb_biz_communitycand:get(Params, Fields).
