%%
%%
-module(empweb_biz_arms).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Списки языков
%%
-export([
    update/1,
    create/1,
    repost/1,
    delete/1,
    get/1,
    get/2
]).


%% ==========================================================================
%% Внешние функции
%% ==========================================================================

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Списки языков
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

repost(Params)->
    empdb_biz_arms:repost(Params).

create(Params)->
    empdb_biz_arms:create(Params).

update(Params)->
    empdb_biz_arms:update(Params).

delete(Params)->
    empdb_biz_arms:delete(Params).

get(Params)->
    empdb_biz_arms:get(Params).

get(Params, Fields)->
    empdb_biz_arms:get(Params, Fields).
