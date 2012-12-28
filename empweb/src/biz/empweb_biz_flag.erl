%%
%%
-module(empweb_biz_flag).

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
    empdb_biz_flag:repost(Params).

create(Params)->
    empdb_biz_flag:create(Params).

update(Params)->
    empdb_biz_flag:update(Params).

delete(Params)->
    empdb_biz_flag:delete(Params).

get(Params)->
    empdb_biz_flag:get(Params).

get(Params, Fields)->
    empdb_biz_flag:get(Params, Fields).
