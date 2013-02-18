%%
%%
-module(empweb_biz_room).

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================


-export([
    get/1,
    get/2,
    get_blogs/1,
    get_posts/1,
    get_photos/1,
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

create(Params)->
    empdb_biz_room:create(Params).

update(Params)->
    empdb_biz_room:update(Params).

join(Params)->
    empdb_biz_room:join(Params).

delete(Params)->
    empdb_biz_room:delete(Params).

add_topic(Params)->
    empdb_biz_room:add_topic(Params).

delete_topic(Params)->
    empdb_biz_room:delete_topic(Params).

count(Params)->
    empdb_biz_room:count(Params).

get_blogs(Params)->
    empdb_biz_room:get_blogs(Params).

get_posts(Params)->
    empdb_biz_room:get_posts(Params).

get_photos(Params)->
    empdb_biz_room:get_photos(Params).

get(Params)->
    empdb_biz_room:get(Params).

get(Params, Fields)->
    empdb_biz_room:get(Params, Fields).

    
