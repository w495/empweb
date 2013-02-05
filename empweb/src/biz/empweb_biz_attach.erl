%%
%%
-module(empweb_biz_attach).

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
    empdb_biz_attach:repost(Params).

create(Params)->
    empdb_biz_attach:create(Params).

update(Params)->
    empdb_biz_attach:update(Params).

delete(Params)->
    empdb_biz_attach:delete(Params).

get(Params)->
    empdb_biz_attach:get(Params).

get(Params, Fields)->
    empdb_biz_attach:get(Params, Fields).
