%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(biz_user).

%%
%% Include files
%%



%%
%% Exported Functions
%%
-export([
    register/1,
    login/1,
    logout/1
]).

%%
%% API Functions
%%

register(Params)->
    domain_user:register([
        {phash, generate_pass(proplists:get_value(pass, Params))}
        |Params
    ]).

login(Params)->
    domain_user:login(Params).

logout(Params)->
    domain_user:logout(Params).


%%
%% Local Functions
%%


generate_pass(Pass) ->
    erlang:list_to_binary(
        lists:append(
            [
                io_lib:format("~2.16.0B", [X]) 
                || X <- binary_to_list(erlang:md5(Pass))
            ]
        )
    ).