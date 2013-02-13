%%
%%
-module(empweb_biz_community).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================


-export([
    get/1,
    get/2,
    get_blogs/1,
    count/1,
    create/1,
    join/1,
    add_topic/1,
    delete_topic/1,
    update/1,
    delete/1
]).


%% ==========================================================================
%% Внешние функции
%% ==========================================================================

get_blogs(Params)->
    empdb_biz_community:get_blogs(Params).

create(Params)->
    empdb_biz_community:create(Params).

update(Params)->
    empdb_biz_community:update(Params).

join(Params)->
    empdb_biz_community:join(Params).

delete(Params)->
    empdb_biz_community:delete(Params).

add_topic(Params)->
    empdb_biz_community:add_topic(Params).

delete_topic(Params)->
    empdb_biz_community:delete_topic(Params).

count(Params)->
    empdb_biz_community:count(Params).

get(Params)->
    empdb_biz_community:get(Params).

get(Params, Fields)->
    empdb_biz_community:get(Params, Fields).

    
