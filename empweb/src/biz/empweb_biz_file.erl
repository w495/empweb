%%
%%
-module(empweb_biz_file).

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

create(Params)->
    io:format("~n~n~n ~p in ~p ~n~n~n", [?MODULE, ?LINE]),
    %io:format("------------------------------------ = ~p = --------------------", [Params]),
    Res = empdb_biz_file:create(Params),
    io:format("~n~n~n ~p in ~p ~n~n~n", [?MODULE, ?LINE]),
    Res.

update(Params)->
    empdb_biz_file:update(Params).

delete(Params)->
    empdb_biz_file:delete(Params).

get(Params)->
    empdb_biz_file:get(Params).

get(Params, Fields)->
    empdb_biz_file:get(Params, Fields).
