%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_room).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
    update/1,
    create/1,
    get/1,
    get/2
]).

%%
%% API Functions
%%


create(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_room:create(Con, Params)
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_room:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_room:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_room:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_room:is_owner(Con, Uid, Oid)
    end).


%%
%% Local Functions
%%

