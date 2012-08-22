%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(domain_doc).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
    update_doctype/1,
    create_doctype/1,
    get_doctype/1,
    get_doctype/2
]).

%%
%% Exported Functions
%%
-export([
    update_acctype/1,
    create_acctype/1,
    get_acctype/1,
    get_acctype/2
]).

%%
%% Exported Functions
%%
-export([
    update_contype/1,
    create_contype/1,
    get_contype/1,
    get_contype/2
]).

%%
%% Exported Functions
%%
-export([
    update_blog/1,
    create_blog/1,
    get_blog/1,
    get_blog/2
]).


%%
%% Exported Functions
%%
-export([
    update_post/1,
    create_post/1,
    get_post/1,
    get_post/2
]).


%%
%% Exported Functions
%%
-export([
    update_room/1,
    create_room/1,
    get_room/1,
    get_room/2
]).



%%
%% Exported Functions
%%
-export([
    update_chatlang/1,
    create_chatlang/1,
    get_chatlang/1,
    get_chatlang/2
]).




%%
%% Exported Functions
%%
-export([
    update_roomtype/1,
    create_roomtype/1,
    get_roomtype/1,
    get_roomtype/2
]).



%%
%% API Functions
%%


create_contype(Params)->
    dao:with_connection(fun(Con)->
        dao_doc:create_contype(Con, Params)
    end).

update_contype(Params)->
    dao:with_connection(fun(Con)->
        dao_doc:update_contype(Con, Params)
    end).

get_contype(Params)->
    dao:with_connection(fun(Con)->
        dao_doc:get_contype(Con, [{isdeleted, false}|Params])
    end).

get_contype(Params, Fileds)->
    dao:with_connection(fun(Con)->
        dao_doc:get_contype(Con, [{isdeleted, false}|Params], Fileds)
    end).




create_doctype(Params)->
    dao:with_connection(fun(Con)->
        dao_doc:create_doctype(Con, Params)
    end).

update_doctype(Params)->
    dao:with_connection(fun(Con)->
        dao_doc:update_doctype(Con, Params)
    end).

get_doctype(Params)->
    dao:with_connection(fun(Con)->
        dao_doc:get_doctype(Con, [{isdeleted, false}|Params])
    end).

get_doctype(Params, Fileds)->
    dao:with_connection(fun(Con)->
        dao_doc:get_doctype(Con, [{isdeleted, false}|Params], Fileds)
    end).




create_acctype(Params)->
    dao:with_connection(fun(Con)->
        dao_doc:create_acctype(Con, Params)
    end).

update_acctype(Params)->
    dao:with_connection(fun(Con)->
        dao_doc:update_acctype(Con, Params)
    end).

get_acctype(Params)->
    dao:with_connection(fun(Con)->
        dao_doc:get_acctype(Con, [{isdeleted, false}|Params])
    end).

get_acctype(Params, Fileds)->
    dao:with_connection(fun(Con)->
        dao_doc:get_acctype(Con, [{isdeleted, false}|Params], Fileds)
    end).



create_blog(Params)->
    dao:with_connection(fun(Con)->
        dao_blog:create(Con, Params)
    end).

update_blog(Params)->
    dao:with_connection(fun(Con)->
        dao_blog:update(Con, Params)
    end).

get_blog(Params)->
    dao:with_connection(fun(Con)->
        dao_blog:get(Con, [{isdeleted, false}|Params])
    end).

get_blog(Params, Fileds)->
    dao:with_connection(fun(Con)->
        dao_blog:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_blog_owner(Uid, Oid)->
    dao:with_connection(fun(Con)->
        dao_blog:is_owner(Con, Uid, Oid)
    end).



create_post(Params)->
    dao:with_connection(fun(Con)->
        dao_post:create(Con, Params)
    end).

update_post(Params)->
    dao:with_connection(fun(Con)->
        dao_post:update(Con, Params)
    end).

get_post(Params)->
    dao:with_connection(fun(Con)->
        dao_post:get(Con, [{isdeleted, false}|Params])
    end).

get_post(Params, Fileds)->
    dao:with_connection(fun(Con)->
        dao_post:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_post_owner(Uid, Oid)->
    dao:with_connection(fun(Con)->
        dao_post:is_owner(Con, Uid, Oid)
    end).



create_room(Params)->
    dao:with_connection(fun(Con)->
        dao_room:create(Con, Params)
    end).

update_room(Params)->
    dao:with_connection(fun(Con)->
        dao_room:update(Con, Params)
    end).

get_room(Params)->
    dao:with_connection(fun(Con)->
        dao_room:get(Con, [{isdeleted, false}|Params])
    end).

get_room(Params, Fileds)->
    dao:with_connection(fun(Con)->
        dao_room:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_room_owner(Uid, Oid)->
    dao:with_connection(fun(Con)->
        dao_room:is_owner(Con, Uid, Oid)
    end).



create_roomtype(Params)->
    dao:with_connection(fun(Con)->
        dao_room:create_roomtype(Con, Params)
    end).

update_roomtype(Params)->
    dao:with_connection(fun(Con)->
        dao_room:update_roomtype(Con, Params)
    end).

get_roomtype(Params)->
    dao:with_connection(fun(Con)->
        dao_room:get_roomtype(Con, [{isdeleted, false}|Params])
    end).

get_roomtype(Params, Fileds)->
    dao:with_connection(fun(Con)->
        dao_room:get_roomtype(Con, [{isdeleted, false}|Params], Fileds)
    end).


create_chatlang(Params)->
    dao:with_connection(fun(Con)->
        dao_room:create_chatlang(Con, Params)
    end).

update_chatlang(Params)->
    dao:with_connection(fun(Con)->
        dao_room:update_chatlang(Con, Params)
    end).

get_chatlang(Params)->
    dao:with_connection(fun(Con)->
        dao_room:get_chatlang(Con, [{isdeleted, false}|Params])
    end).

get_chatlang(Params, Fileds)->
    dao:with_connection(fun(Con)->
        dao_room:get_chatlang(Con, [{isdeleted, false}|Params], Fileds)
    end).


%%
%% Local Functions
%%

