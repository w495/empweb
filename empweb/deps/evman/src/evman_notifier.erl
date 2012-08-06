-module(evman_notifier).


-export([
    start_link/1,
    start_link/2,
    add_handler/3,
    add_sup_handler/3,
    rem_handler/3,
    rem_sup_handler/3,
    get_handlers/1,
    info/2,
    note/2
]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {start_link,0},
        {start_link,1},
        {add_handler, 2},
        {add_guarded_handler, 2},
        {rem_handler, 1},
        {rem_guarded_handler, 1},
        {info, 1},
        {get_handlers, 0}
    ];

behaviour_info(_Other) ->
    undefined.


start_link(Eventname) ->
    gen_event:start_link({local, Eventname}).

start_link(Eventname, Handler_Args) ->
    %%% Handler_Args = [{Handler, Args}, {Handler, Args}]
    Link = gen_event:start_link({local, Eventname}),
    lists:foreach(fun({Handler, Args})->
        Eventname:add_handler(Handler, Args)
    end,Handler_Args),
    Link.

add_handler(Eventname, ModuleName, Args) ->
    ok = gen_event:add_handler(Eventname, ModuleName, Args).

add_sup_handler(Eventname, ModuleName, Args) ->
    ok = gen_event:add_sup_handler(Eventname, ModuleName, Args).

rem_handler(Eventname, ModuleName, Args) ->
    ok = gen_event:delete_handler(Eventname, ModuleName, Args),
    ok.

rem_sup_handler(Eventname, ModuleName, Args) ->
    ok = gen_event:delete_sup_handler(Eventname, ModuleName, Args),
    ok.

get_handlers(Eventname) ->
    gen_event:which_handlers(Eventname).


info(Eventname, Msg) ->
    gen_event:notify(Eventname, {Eventname, Msg}).

note(Eventname, Msg) ->
    gen_event:notify(Eventname, {Eventname, Msg}).
