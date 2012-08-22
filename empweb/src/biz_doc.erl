%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_blog
-module(biz_doc).

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
    get_blog/1,
    get_blog/2,
    create_blog/1,
    update_blog/1
]).

%%
%% Exported Functions
%%
-export([
    get_post/1,
    get_post/2,
    create_post/1,
    update_post/1
]).


%%
%% Exported Functions
%%
-export([
    get_room/1,
    get_room/2,
    create_room/1,
    update_room/1
]).



%%
%% Exported Functions
%%
-export([
    get_roomtype/1,
    get_roomtype/2,
    create_roomtype/1,
    update_roomtype/1
]).



%%
%% Exported Functions
%%
-export([
    get_chatlang/1,
    get_chatlang/2,
    create_chatlang/1,
    update_chatlang/1
]).


%%
%% API Functions
%%



create_doctype(Params)->
    domain_doc:create_doctype(Params).

update_doctype(Params)->
    domain_doc:update_doctype(Params).

get_doctype(Params)->
    domain_doc:get_doctype(Params).

get_doctype(Params, Fields)->
    domain_doc:get_doctype(Params, Fields).



create_acctype(Params)->
    domain_doc:create_acctype(Params).

update_acctype(Params)->
    domain_doc:update_acctype(Params).

get_acctype(Params)->
    domain_doc:get_acctype(Params).

get_acctype(Params, Fields)->
    domain_doc:get_acctype(Params, Fields).



create_contype(Params)->
    domain_doc:create_contype(Params).

update_contype(Params)->
    domain_doc:update_contype(Params).

get_contype(Params)->
    domain_doc:get_contype(Params).

get_contype(Params, Fields)->
    domain_doc:get_contype(Params, Fields).




create_blog(Params)->
    domain_doc:create_blog(Params).

update_blog(Params)->
    domain_doc:update_blog(Params).

get_blog(Params)->
    domain_doc:get_blog(Params).

get_blog(Params, Fields)->
    domain_doc:get_blog(Params, Fields).




create_post(Params)->
    domain_doc:create_post(Params).

update_post(Params)->
    domain_doc:update_post(Params).

get_post(Params)->
    domain_doc:get_post(Params).

get_post(Params, Fields)->
    domain_doc:get_post(Params, Fields).



create_room(Params)->
    domain_doc:create_room(Params).

update_room(Params)->
    domain_doc:update_room(Params).

get_room(Params)->
    domain_doc:get_room(Params).

get_room(Params, Fields)->
    domain_doc:get_room(Params, Fields).



create_roomtype(Params)->
    domain_doc:create_roomtype(Params).

update_roomtype(Params)->
    domain_doc:update_roomtype(Params).

get_roomtype(Params)->
    domain_doc:get_roomtype(Params).

get_roomtype(Params, Fields)->
    domain_doc:get_roomtype(Params, Fields).



create_chatlang(Params)->
    domain_doc:create_chatlang(Params).

update_chatlang(Params)->
    domain_doc:update_chatlang(Params).

get_chatlang(Params)->
    domain_doc:get_chatlang(Params).

get_chatlang(Params, Fields)->
    domain_doc:get_chatlang(Params, Fields).


