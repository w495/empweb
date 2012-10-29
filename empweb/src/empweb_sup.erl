%% Feel free to use, reuse and abuse the code in this file.

-module(empweb_sup).
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
%     Evman_sup = {
%         evman_sup,
%         {evman_sup, start_link, []},
%         permanent,
%         5000,
%         supervisor,
%         [evman_sup]
%     },

    Empweb_biz_session = {
        empweb_biz_session,
        {empweb_biz_session, start_link, []},
        permanent,
        5001,
        supervisor,
        [empweb_biz_session]
    },

    {ok, {{one_for_one, 10, 10}, [
        Empweb_biz_session
    ]}}.

    