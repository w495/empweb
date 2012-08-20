-module(psqlcp_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(RECONNECT_TIMEOUT, 6000).

-record(state, {
    conn,
    hostname,
    database,
    username,
    password,
    timeout=?RECONNECT_TIMEOUT,
    offline=true
}).

start_link(Args) ->
    io:format("Args 1  = ~p~n~n", [Args]),
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    io:format("Args  2 = ~p~n~n", [Args]),
    Hostname = proplists:get_value(hostname, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    State = connect(Hostname, Username, Password, Database),
    io:format("State  = ~p~n~n", [State ]),
    {ok, State, ?RECONNECT_TIMEOUT}.

connect(Hostname, Username, Password, Database)->
    connect(Hostname, Username, Password, Database, ?RECONNECT_TIMEOUT).

connect(Hostname, Username, Password, Database, Timeout)->
    case pgsql:connect(Hostname, Username, Password, [
        {database, Database}
    ]) of
    {ok, Conn}  ->
        #state{
            offline=false,
            conn=Conn,
            hostname=Hostname,
            username=Username,
            password=Password,
            database=Database,
            timeout=Timeout
        };
    {error, _} ->
        #state{
            offline=true,
            hostname=Hostname,
            username=Username,
            password=Password,
            database=Database,
            timeout=Timeout
        }
    end.


handle_call(Request,From,
        #state{
            offline=true,
            hostname=Hostname,
            username=Username,
            password=Password,
            database=Database
        }
    ) ->
    handle_call(
        Request,
        From,
        connect(Hostname, Username, Password, Database)
    );

handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:squery(Conn, Sql), State};
handle_call({equery, Stmt, Params}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:equery(Conn, Stmt, Params), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, #state{
            offline=true,
            hostname=Hostname,
            username=Username,
            password=Password,
            database=Database
        }) ->
    Newstate = connect(Hostname, Username, Password, Database),
    io:format("Newstate   = ~p~n~n", [Newstate ]),
    {noreply, Newstate, Newstate#state.timeout};

handle_info(_Info, State) ->
    {noreply, State, State#state.timeout}.


terminate(_Reason, #state{offline=true}) ->
    ok;

terminate(_Reason, #state{conn=Conn}) ->
    ok = pgsql:close(Conn),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
