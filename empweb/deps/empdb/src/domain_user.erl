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
    update/1,
    login/1,
    logout/1,
    get/1,
    get/2,
    get_opt/2,
    get_opt/3,
    get_friends/1,
    add_friend/1,
    delete_friend/1
]).

%%
%% API Functions
%%

register(Params)->
    dao:with_connection(fun(Con)->
        dao_user:create(Con, Params)
    end).

update(Params)->
    dao:with_connection(fun(Con)->
        dao_user:update(Con, Params)
    end).

login(Params)->
    Nick = proplists:get_value(nick, Params),
    Pass = proplists:get_value(pass, Params),
    dao:with_connection(fun(Con)->
        dao_user:get(Con, [{deleted, false}, {nick, Nick}])
    end).

logout(Params)->
    Nick = proplists:get_value(nick, Params),
    Pass = proplists:get_value(pass, Params),
    dao:with_connection(fun(Con)->
        dao_user:get(Con, [{deleted, false}, {nick, Nick}])
    end).

get({K, V})->
    ?MODULE:get([{K, V}]);

get(all)->
    ?MODULE:get([]);

get(Params)->
    dao:with_connection(fun(Con)->
        dao_user:get(Con, [{deleted, false}|Params])
    end).

get({K, V}, Fileds)->
    ?MODULE:get([{K, V}], Fileds);

get(all, Fileds)->
    ?MODULE:get([], Fileds);

get(Params, Fileds)->
    dao:with_connection(fun(Con)->
        dao_user:get(Con, [{deleted, false}|Params], Fileds)
    end).

get_opt(Params, Fileds, Options)->
    dao:with_connection(fun(Con)->
        {ok, Userpls} = dao_user:get(Con, [{deleted, false}|Params], Fileds),
        get_opt(Con, Params, Options, Userpls)
    end).

get_opt(Params, Options)->
    dao:with_connection(fun(Con)->
        {ok, Userpls} = dao_user:get(Con, [{deleted, false}|Params]),
        get_opt(Con, Params, Options, Userpls)
    end).

get_opt(Con, Params, [], Proplist)
    -> {ok, [{Proplist}]};

get_opt(Con,Params, [Option|Options], [{Acc}])->
    case Option of
        %% ------------------------------------------------------------------
        {perm_list, Spec} when erlang:is_list(Spec) ->
            {ok, Perm_list} = dao_user:get_perm(Con, Params, Spec),
            get_opt(Con, Params, Options, [{perm_list, Perm_list}|Acc]);
        {perm_list, Spec} when erlang:is_atom(Spec) ->
            {ok, Perm_list} = dao_user:get_perm(Con, Params, [Spec]),
            get_opt(Con, Params, Options, [{perm_list, Perm_list}|Acc]);
        perm_list ->
            {ok, Perm_list} = dao_user:get_perm(Con, Params, [name]),
            get_opt(Con, Params, Options, [{perm_list, Perm_list}|Acc]);
        perm_names ->
            {ok, Perm_list} = dao_user:get_perm(Con, Params, [name]),
            Perm_names = lists:map(fun({Permpl})->
                convert:to_atom(proplists:get_value(name, Permpl))
            end, Perm_list),
            get_opt(Con, Params, Options, [{perm_names, Perm_names}|Acc]);
        %% ------------------------------------------------------------------
        {group_list, Spec} when erlang:is_list(Spec) ->
            {ok, Perm_list} = dao_user:get_group(Con, Params, Spec),
            get_opt(Con, Params, Options, [{perm_list, Perm_list}|Acc]);
        {group_list, Spec} when erlang:is_atom(Spec) ->
            {ok, Perm_list} = dao_user:get_group(Con, Params, [Spec]),
            get_opt(Con, Params, Options, [{perm_list, Perm_list}|Acc]);
        group_list ->
            {ok, Perm_list} = dao_user:get_group(Con, Params, [name]),
            get_opt(Con, Params, Options, [{perm_list, Perm_list}|Acc]);
        _ ->
            get_opt(Con, Params, Options, Acc)
    end;

get_opt(Con,Params, [Option|Options], Accs)->
    {ok,
        lists:map(fun({Obj})->
            {ok, [{Result}]} = get_opt(Con,Params, [Option|Options], [{Obj}]),
            {Result}
        end, Accs
    )}.

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

