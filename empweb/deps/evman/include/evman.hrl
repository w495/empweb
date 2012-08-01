-ifndef(__EVMAN_576737140__).
-define(__EVMAN_576737140__, true).

-define(debug(S),
    io:format(
        "debug ~p (~p):" ++ S ++ "~n",
        [calendar:local_time(), ?MODULE]
    )
).

-define(debug(S, P),
    io:format(
        "debug ~p (~p):" ++ S ++ "~n",
        [calendar:local_time(), ?MODULE|P]
    )
).

-define(d(S),
    io:format(
        "debug ~p (~p):" ++ S ++ "~n",
        [calendar:local_time(), ?MODULE]
    )
).
-define(d(S, P),
    io:format(
        "debug ~p (~p):" ++ S ++ "~n",
        [calendar:local_time(), ?MODULE|P]
    )
).

-endif. %%% __EVMAN_576737140__


