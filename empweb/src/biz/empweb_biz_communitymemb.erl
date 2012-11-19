%%
%%
-module(empweb_biz_communitymemb).

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
    empdb_biz_communitymemb:create(Params).

update(Params)->
    empdb_biz_communitymemb:update(Params).

delete(Params)->
    empdb_biz_communitymemb:delete(Params).

get(Params)->
    empdb_biz_communitymemb:get(Params).

get(Params, Fields)->
    empdb_biz_communitymemb:get(Params, Fields).
