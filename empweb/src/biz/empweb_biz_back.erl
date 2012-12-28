%%
%%
-module(empweb_biz_back).

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
    empdb_biz_back:repost(Params).

create(Params)->
    empdb_biz_back:create(Params).

update(Params)->
    empdb_biz_back:update(Params).

delete(Params)->
    empdb_biz_back:delete(Params).

get(Params)->
    empdb_biz_back:get(Params).

get(Params, Fields)->
    empdb_biz_back:get(Params, Fields).
