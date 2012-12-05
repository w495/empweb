-ifndef(__EVENTS_978964407__).
-define(__EVENTS_978964407__, true).



-define(FUNCTION, '__function_macro__').
-define(ARITY, '__function_arity__').
-define(ARGS, '__function_args__').


-define(EVMAN_UNDEFINED, 'evman:undefined').

-record(event_fun, {
    module      =   ?MODULE,
    function,
    arity,
    args,
    line        = ?LINE
}).

-record(event, {
    type        = undefined,
    args        = ?EVMAN_UNDEFINED,
    debug       = ?EVMAN_UNDEFINED,
    info        = ?EVMAN_UNDEFINED,
    notice      = ?EVMAN_UNDEFINED,
    warning     = ?EVMAN_UNDEFINED,
    error       = ?EVMAN_UNDEFINED,
    critical    = ?EVMAN_UNDEFINED,
    alert       = ?EVMAN_UNDEFINED,
    emergency   = ?EVMAN_UNDEFINED,
    comment     = <<>>
}).


-record(evman_note, {
    event_fun   = #event_fun{},
    event       = #event{},
    datetime    = erlang:now()
}).


-define(event_fun(Args),
    #event_fun{
        function    =   ?FUNCTION,
        args        =   Args,
        line        =   ?LINE,
        arity       =   ?ARITY
    }
).

-define(event_fun(), event_fun([])).

-define(evman_note(Event, Args),
    #evman_note{
        event_fun   =   ?event_fun(Args),
        event       =   Event
    }
).

-define(evman_note(Event),  ?evman_note(Event, undefined)).
-define(evman_note(),       ?evman_note(undefined)).

-define(evman(),                evman:info(?evman_note())).
-define(evman(Event),           evman:info(?evman_note(Event))).
-define(evman(Event, Args),     evman:info(?evman_note(Event, Args))).

-define(evman_args(X,       C),
    evman:info(
        ?evman_note(#event{args=X,       comment=C})
    )
).

-define(evman_debug(X,      C),
    evman:info(
        ?evman_note(#event{debug=X,      comment=C})
    )
).

-define(evman_info(X,       C), evman:info(?evman_note(#event{type=info,        info=X,       comment=C}))).
-define(evman_notice(X,     C), evman:info(?evman_note(#event{type=notice,      notice=X,     comment=C}))).
-define(evman_warning(X,    C), evman:info(?evman_note(#event{type=warning,     warning=X,    comment=C}))).
-define(evman_error(X,      C), evman:info(?evman_note(#event{type=error,       error=X,      comment=C}))).
-define(evman_critical(X,   C), evman:info(?evman_note(#event{type=critical,    critical=X,   comment=C}))).
-define(evman_alert(X,      C), evman:info(?evman_note(#event{type=alert,       alert=X,      comment=C}))).
-define(evman_emergency(X,  C), evman:info(?evman_note(#event{type=emergency,   emergency=X,  comment=C}))).

-define(evman_args(X),      ?evman_args(X,       <<"">>)).
-define(evman_debug(X),     ?evman_debug(X,      <<"">>)).
-define(evman_info(X),      ?evman_info(X,       <<"">>)).
-define(evman_notice(X),    ?evman_notice(X,     <<"">>)).
-define(evman_warning(X),   ?evman_warning(X,    <<"">>)).
-define(evman_error(X),     ?evman_error(X,      <<"">>)).
-define(evman_critical(X),  ?evman_critical(X,   <<"">>)).
-define(evman_alert(X),     ?evman_alert(X,      <<"">>)).
-define(evman_emergency(X), ?evman_emergency(X,  <<"">>)).


% 
% %%
% %% event dirty
% %%
% -define(event_d(), ?event()).
% -define(event_d(Event),
%     ?event(#event{Event})
% ).
% 
% 
% -define(evman_note(), evman:note(?evman_note())).
% -define(evman_note(Event), evman:note(?evman_note(Event))).
% 
% -define(evman_funcall(Args), ?evman_note(#event{args=Args})).
% 
%     
% -define(evman_note_d(), evman:note(?event_d())).
% -define(evman_note_d(Event), evman:note(?event_d(Event))).
% 




-endif. %%% __EVENTS_978964407__


