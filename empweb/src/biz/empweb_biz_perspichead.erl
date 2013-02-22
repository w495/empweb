%%
%%
-module(empweb_biz_perspichead).

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
    empdb_biz_perspichead:repost(Params).

create(Params)->
    empdb_biz_perspichead:create(Params).

update(Params)->
    empdb_biz_perspichead:update(Params).

delete(Params)->
    empdb_biz_perspichead:delete(Params).

get(Params)->
    empdb_biz_perspichead:get(Params).

get(Params, Fields)->
    empdb_biz_perspichead:get(Params, Fields).
