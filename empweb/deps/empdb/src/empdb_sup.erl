%% @copyright 2013 Empire
%% @author Илья w-495 Никитин
%% @doc Модуль наблюдения.
%%

-module(empdb_sup).
-behaviour(supervisor).

-export([start_link/0]). %% API.
-export([init/1]). %% supervisor.

-define(SUPERVISOR, ?MODULE).

%% API.
-spec start_link() -> {ok, Pid::pid()}.

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init([]) ->
    %% Описание запуска пула соединений с БД.
    Psqlcp = {
        psqlcp,
        {psqlcp, start_link, []},
        permanent,
        5000,
        supervisor,
        [psqlcp]
    },
    %% Описание запуска кеширования строк запросов.
    %% Это кеширование нужно для быстрого создания и восстановления
    %%  строк запросов к БД. Сами данные при этом не кешируются.
    Dao_static_cashe = {
        empdb_dao_static_cashe,
        {term_cache_ets, start_link, [[
            {'size', '40Mb'},
            {name, empdb_dao_static_cashe}
        ]]},
        permanent,
        5000,
        worker,
        [empdb_dao_static_cashe]
    },
    %% Описание запуска  таймера.
    Empdb_timer =  {
        empdb_timer,
        {empdb_timer, start_link, []},
        permanent,
        5000,
        worker,
        [empdb_timer]
    },

    {ok, {{one_for_one, 10, 10}, [
        Psqlcp,
        Dao_static_cashe,
        Empdb_timer
    ]}}.
