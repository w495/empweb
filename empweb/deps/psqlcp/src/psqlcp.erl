-module(psqlcp).
-behaviour(application).
-behaviour(supervisor).

-export([start/0, start_link/0, start_link/1,stop/0, squery/2, equery/2, equery/3, with_transaction/2, with_connection/2]).
-export([start/2, stop/1]).
-export([init/1]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
    start_link().

get_application() ->
    case application:get_application() of
        undefined ->    psqlcp;
        {ok, App} ->    App
    end.

start_link() ->
    start_link(get_application()).

start_link(Application) ->
    supervisor:start_link({local, psqlcp_sup}, ?MODULE, [Application]).

stop(_State) ->
    ok.

init([]) ->
    init([get_application()]);

init([Application]) ->
    {ok, Pools} = application:get_env(Application, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, psqlcp_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),    
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

%%%
%%% -----------------------------------------------------------------------
%%%

with_connection(Pool, Function) when erlang:is_function(Function, 1) ->
    poolboy:transaction(Pool, fun(Conn) ->
        erlang:apply(Function, [Conn])
    end).

with_transaction(Pool, Function) when erlang:is_function(Function, 1) ->
    poolboy:transaction(Pool, fun(Conn) ->
        ?MODULE:squery(Conn, <<"begin">>),
        Ret = erlang:apply(Function, [Conn]),
        ?MODULE:squery(Conn, <<"commit">>),
        Ret
    end).

%%%
%%% -----------------------------------------------------------------------
%%%

equery(Conn, Stmt) when erlang:is_pid(Conn) ->
    equery({conn, Conn}, Stmt, []);

equery(Pool, Stmt) when erlang:is_atom(Pool) ->
    equery({pool, Pool}, Stmt, []);

equery({conn, Conn}, Stmt) when erlang:is_pid(Conn) ->
    gen_server:call(Conn, {equery, Stmt, []});

equery({pool, Pool}, Stmt)  ->
    poolboy:transaction(Pool, fun(Conn) ->
        equery({conn, Conn}, Stmt, [])
    end).

%%%
%%% -----------------------------------------------------------------------
%%%

equery(Conn, Stmt, Params) when erlang:is_pid(Conn) ->
    equery({conn, Conn}, Stmt, Params);

equery(Pool, Stmt, Params) when erlang:is_atom(Pool) ->
    equery({pool, Pool}, Stmt, Params);

equery({conn, Conn}, Stmt, Params) when erlang:is_pid(Conn) ->
    gen_server:call(Conn, {equery, Stmt, Params});

equery({pool, Pool}, Stmt, Params)  ->
    poolboy:transaction(Pool, fun(Conn) ->
        equery({conn, Conn}, Stmt, Params)
    end).

%%%
%%% -----------------------------------------------------------------------
%%%

squery(Conn, Sql) when erlang:is_pid(Conn) ->
    squery({conn, Conn}, Sql);

squery(Pool, Sql) when erlang:is_atom(Pool) ->
    squery({pool, Pool}, Sql);

squery({conn, Conn}, Sql) ->
    gen_server:call(Conn, {equery, Sql});

squery({pool, Pool}, Sql) ->
    poolboy:transaction(Pool, fun(Conn) ->
        gen_server:call(Conn, {squery, Sql})
    end).

%%%
%%% -----------------------------------------------------------------------
%%%
