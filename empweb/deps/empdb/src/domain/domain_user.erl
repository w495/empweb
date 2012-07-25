%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(domain_user).

%%
%% Include files
%%

-include("user.hrl").

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
    dao:with_connection(fun(Con)->
        dao_user:create(Con, Params)
    end).

login(Params)->
    Nick = proplists:get_value(nick, Params),
    Pass = proplists:get_value(pass, Params),
    dao:with_connection(fun(Con)->
        dao_user:get(Con, {nick, Nick})
    end).

logout(Params)->
    Nick = proplists:get_value(nick, Params),
    Pass = proplists:get_value(pass, Params),
    dao:with_connection(fun(Con)->
        dao_user:get(Con, {nick, Nick})
    end).

%%%
%%%
%%%

add_friend(Params)->
    dao:with_connection(fun(Con)->
        dao_user:add_friend(Con, Params)
    end).

delete_friend(Params)->
    dao:with_connection(fun(Con)->
        dao_user:delete_friend(Con, Params)
    end).

get_friends(Params)->
    dao:with_connection(fun(Con)->
        dao_user:get_friends(Con, Params)
    end).

%%
%% Local Functions
%%

