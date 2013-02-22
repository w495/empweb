%%
%%
-module(empweb_biz_perspicbody).

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
    empdb_biz_perspicbody:repost(Params).

create(Params)->
    empdb_biz_perspicbody:create(Params).

update(Params)->
    empdb_biz_perspicbody:update(Params).

delete(Params)->
    empdb_biz_perspicbody:delete(Params).

get(Params)->
    empdb_biz_perspicbody:get(Params).

get(Params, Fields)->
    empdb_biz_perspicbody:get(Params, Fields).
