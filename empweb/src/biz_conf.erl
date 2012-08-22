%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_blog
-module(biz_conf).

%%
%% Include files
%%

-export([
    update_tr/1,
    create_tr/1,
    get_tr/1,
    get_tr/2
]).

%%
%% Exported Functions
%%
-export([
    update_lang/1,
    create_lang/1,
    get_lang/1,
    get_lang/2
]).


%%
%% API Functions
%%

create_lang(Params)->
    domain_lang:create_lang(Params).

update_lang(Params)->
    domain_lang:update_lang(Params).

get_lang(Params)->
    domain_lang:get_lang(Params).

get_lang(Params, Fields)->
    domain_lang:get_lang(Params, Fields).



create_tr(Params)->
    domain_lang:create_tr(Params).

update_tr(Params)->
    domain_lang:update_tr(Params).

get_tr(Params)->
    domain_lang:get_tr(Params).

get_tr(Params, Fields)->
    domain_lang:get_tr(Params, Fields).


