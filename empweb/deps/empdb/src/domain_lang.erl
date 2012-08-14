%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(domain_lang).

%%
%% Include files
%%

-include("user.hrl").

%%
%% Exported Functions
%%
-export([
    update/1,
    get/1,
    get/2,
    get_opt/2,
    get_opt/3
]).

%%
%% API Functions
%%


update(Params)->
    dao:with_connection(fun(Con)->
        dao_user:update(Con, Params)
    end).

get(Params)->
    dao:with_connection(fun(Con)->
        dao_user:get(Con, Params)
    end).

get(Params, Fileds)->
    dao:with_connection(fun(Con)->
        dao_user:get(Con, Params, Fileds)
    end).

get_opt(Params, Fileds, Options)->
    dao:with_connection(fun(Con)->
        {ok, Userpls} = dao_user:get(Con, Params, Fileds),
        get_opt(Con, Params, Options, Userpls)
    end).

get_opt(Params, Options)->
    dao:with_connection(fun(Con)->
        {ok, Userpls} = dao_user:get(Con, Params),
        get_opt(Con, Params, Options, Userpls)
    end).

get_opt(Con, Params, [], Proplist) ->
    {ok, [{Proplist}]};

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

%%
%% Local Functions
%%

