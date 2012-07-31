%% Feel free to use, reuse and abuse the code in this file.

-module(empdb_sup).
-behaviour(supervisor).

-export([start_link/0]). %% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
    %%% Соединение с локальной базой данных
    Psqlcp = {
        psqlcp,
        {psqlcp, start_link, []},
        permanent,
        5000,
        supervisor,
        [psqlcp]
    },
    
	{ok, {{one_for_one, 10, 10}, [Psqlcp ]}}.
