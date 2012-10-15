-ifndef(__EMPDB_3024595043__).
-define(__EMPDB_3024595043__, true).

%%
%% @doc Структура описания запроса.
%%
-record(queryobj, {
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



-ifndef(debug).
    -define(debug(F),  spawn_link(fun()-> io:format(F) end)).
    -define(debug(F, P),  spawn_link(fun()-> io:format(F, P) end)).
%     -define(debug(F),  spawn_link(fun()-> ok end)).
%     -define(debug(F, P),  spawn_link(fun()-> ok end)).
-endif.

-endif. %%% __EMPDB_3024595043__






