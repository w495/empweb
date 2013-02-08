%%% @file empdb_timer.erl
%%%
%%% Этому модулю никто не посылает запросов.
%%% Используется как таймер.
%%% События происходят по истечению
%%%     EMPDB_TIMER_TIMEOUT
%%% и обрабатываются в handle_info(timeout, State)
%%%
%%%

-module(empdb_timer).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("empdb.hrl").
%% ----------------------------------------------------------------------------
%% Defines
%% ----------------------------------------------------------------------------

-define(EMPDB_TIMER_TIMEOUT, 300000). %% 5*60*1000
%-define(EMPDB_TIMER_TIMEOUT, 5000). %% 5*60*1000


%% ----------------------------------------------------------------------------
%% External exports
%% ----------------------------------------------------------------------------
-export([start_link/0]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================
start_link()->
    gen_server:start_link(
        {local, ?MODULE}, ?MODULE, [],
        [
            {spawn_opt,[{min_heap_size,200000}]}
        ]
    ).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: []
%% --------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, [], ?EMPDB_TIMER_TIMEOUT}.


%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call(Request, _From, State) ->
    {noreply, State, ?EMPDB_TIMER_TIMEOUT}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(Msg, State) ->
    {noreply, State, ?EMPDB_TIMER_TIMEOUT}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State) ->
    timeouts(),
    {noreply, State, ?EMPDB_TIMER_TIMEOUT};

handle_info(Info, State) ->
    {noreply, State, ?EMPDB_TIMER_TIMEOUT}.

timeouts()->
    empdb_biz_roomlot:timeout(),
    empdb_biz_exile:timeout(),
    empdb_biz_invisbuy:timeout(),
    empdb_biz_zprotbuy:timeout(),
    ok.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Local
%% --------------------------------------------------------------------

