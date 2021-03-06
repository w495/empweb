%%
%%
-module(empweb_biz_album).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Списки языков
%%
-export([
    update/1,
    create/1,
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

create(Params)->
    empdb_biz_album:create(Params).

update(Params)->
    empdb_biz_album:update(Params).

delete(Params)->
    empdb_biz_album:delete(Params).

get(Params)->
    empdb_biz_album:get(Params).

get(Params, Fields)->
    empdb_biz_album:get(Params, Fields).
