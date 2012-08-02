-ifndef(__EVENTS_978964407__).
-define(__EVENTS_978964407__, true).



-define(FUNCTION, '__function_macro__').
-define(ARITY, '__function_arity__').
-define(ARGS, '__function_args__').

-record(event_fun, {
    module      =   ?MODULE,
    function,
    arity,
    line        =   ?LINE
}).

-record(event, {
    type,
    args    = [],
    data    = [],
    log     = [],
    debug   = [],
    error   = [],
    info    = [],
    mess    = <<>>
}).


-record(evman_note, {
   'fun' = #event_fun{},
    event = #event{},
    datetime = erlang:now()
}).


-define(event(),
    #evman_note{
        'fun'=#event_fun{function=?FUNCTION, arity=?ARITY, args}
    }
).

-define(event(Event),
    #evman_note{
        'fun'   = #event_fun{function=?FUNCTION, arity=?ARITY},
         event   = Event
    }
).


%%
%% event dirty
%%
-define(event_d(), ?event()).
-define(event_d(Event),
    ?event(#event{Event})
).


-define(evman_note(), evman:note(?event())).
-define(evman_note(Event), evman:note(?event(Event))).

-define(evman_funcall(Args), ?evman_note(#event{args=Args, type=function})).

    
-define(evman_note_d(), evman:note(?event_d())).
-define(evman_note_d(Event), evman:note(?event_d(Event))).





-endif. %%% __EVENTS_978964407__


