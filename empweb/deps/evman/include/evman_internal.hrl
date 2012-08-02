-ifndef(__EVMAN_INTERNAL_288850483__).
-define(__EVMAN_INTERNAL_288850483__, true).

-include("events.hrl").

%%% =====================================================================
%%% =====================================================================

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

%%% =====================================================================
%%% =====================================================================


-endif. %%% __EVMAN_INTERNAL_288850483__


