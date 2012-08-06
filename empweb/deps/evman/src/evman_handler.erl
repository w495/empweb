%%%
%%% @file evman_gen_handler.erl
%%%   Абстрактный обработчик событий.
%%%   Можно переобределить его методы с помощью.
%%%
%%%     -behaviour(gen_event).
%%%     -extends(evman_gen_handler).
%%%

-module(evman_handler).

-include("evman.hrl").


-compile({parse_transform, lager_transform}).
  
-define(HANDLERNAME, ?MODULE).

-export([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {}).

init([]) ->
  {ok, #state{}}.

%%%
%%% -----------------------------------------------------------------------
%%%

handle_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    event=
        #event{
            args        = Args,
            error       = Error,
            comment     = Comment
        }
    }}, State) when Error =/=  ?EVMAN_UNDEFINED->
    evman:llog(error,
        "~n"
        "   call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "   event:~n"
        "       ~p~n",
        [   Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Error
        ]
    ),
  {ok, State};

handle_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    event=
        #event{
            args        = Args,
            warning     = Warning,
            comment     = Comment
        }
    }}, State) when Warning =/=  ?EVMAN_UNDEFINED->
    evman:llog(warning,
        "~n"
        "   call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "   event:~n"
        "       ~p~n",
        [   Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Warning
        ]
    ),
  {ok, State};

handle_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    event=
        #event{
            args        = Args,
            notice      = Notice,
            comment     = Comment
        }
    }}, State) when Notice =/=  ?EVMAN_UNDEFINED->
    evman:llog(notice,
        "~n"
        "   call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "   event:~n"
        "       ~p~n",
        [   Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Notice
        ]
    ),
  {ok, State};

handle_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    event=
        #event{
            args        = Args,
            info        = Info,
            comment     = Comment
        }
    }}, State) when Info =/=  ?EVMAN_UNDEFINED->
    evman:llog(info,
        "~n"
        "   call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "   event:~n"
        "       ~p~n",
        [   Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Info
        ]
    ),
  {ok, State};

handle_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    event=
        #event{
            args        = Args,
            debug       = Debug,
            comment     = Comment
        }
    }}, State) when Debug =/=  ?EVMAN_UNDEFINED->
    evman:llog(debug,
        "~n"
        "   call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "   event:~n"
        "       ~p~n",
        [   Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Debug
        ]
    ),
  {ok, State};


handle_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    event=
        #event{
            args        = Args,
            comment     = Comment
        }
    }}, State) when Args =/=  ?EVMAN_UNDEFINED->
    evman:llog(info,
        "~n"
        "   was function call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n",
        [   Line,
            Module,
            Function,
            Arity,
            Args,
            Comment     ]
    ),
  {ok, State};



handle_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    event=
        #event{
            args        = Args,
            debug       = Debug,
            info        = Info,
            notice      = Notice,
            warning     = Warning,
            error       = Error,
            critical    = Critical,
            alert       = Alert,
            emergency   = Emergency,
            comment     = Comment
        }
    }}, State) ->
    evman:llog(debug,
        "~n"
        "   call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "   event:~n"
        "       debug    :~n"
        "           ~p~n    "
        "       info     :~n"
        "           ~p~n    "
        "       notice   :~n"
        "           ~p~n    "
        "       warning  :~n"
        "           ~p~n    "
        "       error    :~n"
        "           ~p~n    "
        "       critical :~n"
        "           ~p~n    "
        "       alert    :~n"
        "           ~p~n    "
        "       emergency:~n"
        "           ~p~n    ",
        [   Comment,
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Debug,
            Info,
            Notice,
            Warning,
            Error,
            Critical,
            Alert,
            Emergency
        ]
    ),
  {ok, State};

  
handle_event({_sender, Eevent}, State) ->
    %?debug("~p ~p has event from ~p, `~p' and state `~p' ~n",
    %  [?HANDLERNAME, self(), _sender, _event, State]),
    evman:llog(debug, "~p", [Eevent]),
  {ok, State};

handle_event(_event, State) ->
  %?debug("~p ~p has event `~p' and state `~p' ~n",
  %  [?HANDLERNAME, self(), _event, State]),
  {ok, State}.

%%%
%%% -----------------------------------------------------------------------
%%%

handle_call(_request, State) ->
  %?debug("~p ~p has call `~p' and state `~p' ~n",
  %  [?HANDLERNAME, self(), _request, State]),
  {ok, ok, State}.

handle_info(_info, State) ->
  %   ?debug("~p ~p has info `~p' and state `~p' ~n",
  %     [?HANDLERNAME, self(), _info, State]),
  {ok, State}.

terminate(_reason, _state) ->
  %?debug("~p ~p was terminate with reason `~p' and state `~p' ~n",
  %  [?HANDLERNAME, self(), _reason, _state]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



