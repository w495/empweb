%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_blog
-module(biz_blog).

%%
%% Include files
%%


%%
%% Exported Functions
%%
-export([
    update/1
]).

%%
%% API Functions
%%


create(Params)->
    domain_blog:create(Params).

update(Params)->
    domain_blog:update(Params).


get(Params)->
    domain_blog:get(Params).

