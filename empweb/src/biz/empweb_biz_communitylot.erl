%%
%%
-module(empweb_biz_communitylot).

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
    empdb_biz_communitylot:create(Params).

update(Params)->
    empdb_biz_communitylot:update(Params).

delete(Params)->
    empdb_biz_communitylot:delete(Params).

get(Params)->
    empdb_biz_communitylot:get(Params).

get(Params, Fields)->
    empdb_biz_communitylot:get(Params, Fields).
