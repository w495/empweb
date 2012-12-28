%%
%%
-module(empweb_biz_wall).

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
    empdb_biz_wall:repost(Params).

create(Params)->
    empdb_biz_wall:create(Params).

update(Params)->
    empdb_biz_wall:update(Params).

delete(Params)->
    empdb_biz_wall:delete(Params).

get(Params)->
    empdb_biz_wall:get(Params).

get(Params, Fields)->
    empdb_biz_wall:get(Params, Fields).
