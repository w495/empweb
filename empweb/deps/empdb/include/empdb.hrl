-ifndef(__EMPDB_3024595043__).
-define(__EMPDB_3024595043__, true).

%%
%% @doc Структура описания запроса.
%%
-record(queryobj, {
        distinct    = [] :: list(),
    %% Для select \ insert \ update \ delete
        filter      = [] :: proplists:proplist(),
    %% Для select \ insert \ update
        fields      = [] :: list(),
    %% Для insert \ update
        values      = [] :: proplists:proplist(),
    %% Для select
        order       = [] :: atom() | list() | {asc|desc, any()},
    %% Для select
        limit       = undefined :: undefined | integer(),
    %% Для select
        offset      = undefined :: undefined | integer()
}).



-ifndef(empdb_debug).
    -define(empdb_debug(F),     spawn_link(fun()-> io:format(F) end)).
    -define(empdb_debug(F, P),  spawn_link(fun()-> io:format(F, P) end)).
-endif.


% 
%     -define(empdb_debug(F),  spawn_link(fun()-> ok end)).
%     -define(empdb_debug(F, P),  spawn_link(fun()-> ok end)).

-endif. %%% __EMPDB_3024595043__




-define(EMPDB_UNIXTIMEWEEK,  604800). % 60*60*24*7.
-define(EMPDB_UNIXTIMEDAY,   86400). % 60*60*24*7.


-define(EMPDB_BIZ_ROOMBET_EPSILON, 0.001).