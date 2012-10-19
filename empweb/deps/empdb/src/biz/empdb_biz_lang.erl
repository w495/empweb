%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_lang).


%% Exported Functions
%%

%%
%% Exported Functions
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


create_tr(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_tr:create(Con, Params)
    end).

update_tr(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_tr:update(Con, Params)
    end).

get_tr(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_tr:get(Con, [{isdeleted, false}|Params])
    end).

get_tr(Params, Fileds)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_tr:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_tr_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_tr:is_owner(Con, Uid, Oid)
    end).

create_lang(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_lang:create(Con, Params)
    end).

update_lang(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_lang:update(Con, Params)
    end).

get_lang(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_lang:get(Con, [{isdeleted, false}|Params])
    end).

get_lang(Params, Fileds)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_lang:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_lang_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_lang:is_owner(Con, Uid, Oid)
    end).


%%
%% Local Functions
%%

