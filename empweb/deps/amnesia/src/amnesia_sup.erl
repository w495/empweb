%% Feel free to use, reuse and abuse the code in this file.

-module(amnesia_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    start_link/1
]).

%% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, Pid::pid()}.

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

start_link(Options) ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, [Options]).


init([]) ->
    init_([]);

init([Options]) ->
    init_(Options).

init_(Options) ->
    Amnesia_config = {
        amnesia_config,
        {term_cache_ets, start_link, [[
            {'size', '4Mb'},
            {name, amnesia_config}
        ]]},
        permanent,
        5000,
        worker,
        [amnesia_config]
    },
    
    Amnesia_worker = {
        amnesia_worker,
        {amnesia_worker, start_link, [Options]},
        permanent,
        5000,
        worker,
        [amnesia_worker]
    },

    
    {ok, {{one_for_all, 10, 10}, [
        Amnesia_config,
        Amnesia_worker
    ]}}.
