%% Feel free to use, reuse and abuse the code in this file.

-module(empweb_handler_debug_logs_ws).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).


%% Определения общие для всего приложения
%%
-include("empweb.hrl").

%%
%% Описание записей событий и макросов
%%
-include_lib("evman/include/events.hrl").


%%
%% Трансформация для получения имени функции.
%%
-include_lib("evman/include/evman_transform.hrl").


%%
%% =========================================================================
%% =========================================================================
%%


-export([
    init/3,
    handle/2,
    terminate/2
]).

-export([
    websocket_init/3,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).



-define(HANDLERNAME, ?MODULE).


-define(HANDLERMODULE, empweb_biz_debug_logs_ws).


-record(state, {
    level,
    storage,
    event
}).


init({_Any, http}, Req, Options) ->
    ?debug("(1)~n"),
    case cowboy_http_req:header('Upgrade', Req) of
        {undefined, Req2} -> {ok, Req2, Options };
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],[], Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

level_to_atom(<<"debug">>) ->
    debug;

level_to_atom(_) ->
    all.

websocket_init(_Any, Req, _State) ->
    timer:send_interval(1000, tick),
    {ok, cowboy_http_req:compact(Req), #state{}}.

websocket_handle({text, Msg}, Req, State) ->
    Level = level_to_atom(Msg),
    ?HANDLERMODULE:start([{level, Level}]),
    {reply,
        {text, base64:encode(<< "You said: ", Msg/binary >>)},
        Req,
        State#state{level=Level }
    };

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info(tick, Req, #state{event=undefined} = State) ->
    {ok, Req, State#state{event = []}};

websocket_info(tick, Req, #state{event=[]} = State) ->
    {ok, Req, State#state{event = get_events(State)}};

websocket_info(tick, Req, #state{event = Events} = State) ->
    Result = list_events(Events),
    {reply, {text, Result}, Req, State#state{event=[]}};

websocket_info(_Info, Req, #state{} = State) ->

    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _) ->
    ?HANDLERMODULE:stop(),
    ok.

get_events(#state{level=Level} = State)->
    lists:map(
        fun(Event)->
            format_event(Event, State)
        end,
        ?HANDLERMODULE:get_events()
    ).
    
% get_events(#state{level=Level} = State)->
%     case ?HANDLERMODULE:get_events() of
%         {error,bad_module} ->
%             ?HANDLERMODULE:start([{level, Level}]),
%             [];
%         Events ->
%             lists:map(
%                 fun(Event)->
%                     format_event(Event, State)
%                 end,
%                 Events
%             )
%     end.

format_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    datetime=Datetime,
    event=
        #event{
            args        = Args,
            error       = Error,
            comment     = Comment
        }
    }}, _State) when Error =/=  ?EVMAN_UNDEFINED->
    io_lib:format(
        "<hr/><div style='color:red'>ERROR ~p:<pre><code>"
        "   call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "   event (error):~n"
        "       ~p~n"
        "</code></pre></div>",
        [   unixtime(Datetime),
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Error
        ]
    );

format_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    datetime=Datetime,
    event=
        #event{
            args        = Args,
            warning     = Warning,
            comment     = Comment
        }
    }}, _State) when Warning =/=  ?EVMAN_UNDEFINED->
    io_lib:format(
        "<hr/><div style='color:orange'>WARNING ~p:<pre><code>"
        "   call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "   event (warning):~n"
        "       ~p~n"
        "</code></pre></div>",
        [   unixtime(Datetime),
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Warning
        ]
    );

format_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    datetime=Datetime,
    event=
        #event{
            args        = Args,
            notice      = Notice,
            comment     = Comment
        }
    }}, _State) when Notice =/=  ?EVMAN_UNDEFINED->
    io_lib:format(
        "<hr/><div style='color:green'>NOTICE ~p:<pre><code>"
        "   call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "   event (notice):~n"
        "       ~p~n"
        "</code></pre></div>",
        [   unixtime(Datetime),
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Notice
        ]
    );

format_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    datetime=Datetime,
    event=
        #event{
            args        = Args,
            info        = Info,
            comment     = Comment
        }
    }}, _State) when Info =/=  ?EVMAN_UNDEFINED->
    io_lib:format(
        "<hr/><div style='color:green'>INFO ~p:<pre><code>"
        "   call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "   event (info):~n"
        "       ~p~n"
        "</code></pre></div>",
        [   unixtime(Datetime),
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Info
        ]
    );

format_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    datetime=Datetime,
    event=
        #event{
            args        = Args,
            debug       = Debug,
            comment     = Comment
        }
    }}, _State) when Debug =/=  ?EVMAN_UNDEFINED->
    io_lib:format(
        "<hr/><div style='color:gray'>DEBUG ~p:<pre><code>"
        "   call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "   event (debug):~n"
        "       ~p~n"
        "</code></pre></div>",
        [   unixtime(Datetime),
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Debug
        ]
    );


format_event({_sender, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    datetime=Datetime,
    event=
        #event{
            args        = Args,
            comment     = Comment
        }
    }}, _State) when Args =/=  ?EVMAN_UNDEFINED->
    io_lib:format(
        "<hr/><div style='color:gray'>FUNCTION CALL ~p:<pre><code>"
        "   was function call:~n"
        "       line    : ~p~n"
        "       module  : ~p~n"
        "       function: ~p~n"
        "       arity   : ~p~n"
        "       args    : ~p~n"
        "   comment: ~n"
        "       ~p~n"
        "</code></pre></div>",
        [   unixtime(Datetime),
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment     ]
    );


format_event({_, #evman_note{
    event_fun=
        #event_fun{
            module      = Module,
            function    = Function,
            arity       = Arity,
            line        = Line
        },
    datetime=Datetime,
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
    } = _Event}, _State) ->
    io_lib:format(
        "<hr/><div>EVENT ~p:<pre><code>"
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
        "       emergency:"
        "           ~p~n    "
        "</code></pre></div>",
        [   unixtime(Datetime),
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
    );

  
format_event([{_, Event}], _State)->
    io_lib:format("<hr/><p> ~p </p>", [Event]);
    
format_event(Event, _State)->
    io_lib:format("<hr/><p> ~p </p>", [Event]).

list_events(Events)->
    lists:map(
        fun(X) ->
            base64:encode(erlang:list_to_binary(X))
        end,
        Events
    ).




unixtime({Ma, S, Mi}) ->
    {Ma * 1000000 +  S +  Mi * 0.000001, calendar:now_to_datetime({Ma, S, Mi})}.
    