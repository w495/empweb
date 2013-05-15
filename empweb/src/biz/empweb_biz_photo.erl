%%
%%
-module(empweb_biz_photo).

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
    get/2,
    get_top/1
]).


%% ==========================================================================
%% Внешние функции
%% ==========================================================================

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Списки языков
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

repost(Params)->
    empdb_biz_photo:repost(Params).

create(Params)->
    empdb_biz_photo:create(Params).

update(Params)->
    empdb_biz_photo:update(Params).

delete(Params)->
    empdb_biz_photo:delete(Params).


get_top(Params)->
    empdb_biz_photo:get_top(Params).

get(Params)->
    empdb_biz_photo:get(Params).

get(Params, Fields)->
    empdb_biz_photo:get(Params, Fields).
