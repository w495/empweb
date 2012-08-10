%% Feel free to use, reuse and abuse the code in this file.

-module(handler_debug_logs_ws).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-behaviour(gen_event).
-compile({parse_transform, lager_transform}).

-include("empweb.hrl").

%%
%% Описание записей событий и макросов
%%
-include_lib("evman/include/events.hrl").



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

-export([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  code_change/3
]).


-define(HANDLERNAME, ?MODULE).

-record(state, {
    storage,
    event
}).

init([]) ->
    {ok, #state{}};

init([State]) ->
    {ok, State}.


init({_Any, http}, Req, Options) ->
    ?debug("(1)~n"),
    case cowboy_http_req:header('Upgrade', Req) of
        {undefined, Req2} -> {ok, Req2, Options };
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->
    ?debug("(2.0)~n"),

    {Host, Req} = cowboy_http_req:raw_host(Req),
    {Port, Req} = cowboy_http_req:port(Req),



    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],

%% HTML code taken from misultin's example file.
[<<"<html>
<head>
<script type=\"text/javascript\">
function msg(text){
    var date = new Date();
    document.getElementById('status').innerHTML
        = \"\" + date + \": \" + text + \"<br/>\"
          + document.getElementById('status').innerHTML;
}
function rpl(text){
    var date = new Date();
    document.getElementById('status').innerHTML
        =   \"<p>\" + date + \" server :\" + text + \"</p>\"
            + document.getElementById('status').innerHTML;
}
function ready(){
    if (\"MozWebSocket\" in window) {
        WebSocket = MozWebSocket;
    }
    if (\"WebSocket\" in window) {
        // browser supports websockets
        var ws = new WebSocket(\"ws://">>, Host, <<":">>, convert:to_list(Port), <<"/.debug/.logs/.ws\");
        ws.onopen = function() {
            // websocket is connected
            msg(\"log websocket connected!\");
            // send hello data to server.
            ws.send(\"give me log!\");
        };
        ws.onmessage = function (evt) {
            var receivedMsg = evt.data;
            rpl(receivedMsg);
        };
        ws.onclose = function() {
            // websocket was closed
            msg(\"websocket was closed\");
        };
    } else {
        // browser does not support websockets
        msg(\"sorry, your browser does not support websockets.\");
    }
}
</script>
</head>
<body onload=\"ready();\">
<div id=\"status\"></div>
</body>
</html>">>], Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, _State) ->
    ?debug("(3)~n"),
    timer:send_interval(1000, tick),
    Req2 = cowboy_http_req:compact(Req),
    ?debug("(4)~n"),
    State = #state{storage=ets:new(?HANDLERNAME, [set, public, {write_concurrency,true}])},
    ?debug("(4)~n"),
    evman:add_handler(?HANDLERNAME, [State]),
    ?debug("(4)~n"),
    {ok, Req2, State}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "You said: ", Msg/binary >>}, Req, State};

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.


websocket_info(tick, Req, #state{event=undefined} = State) ->
    {ok, Req, State#state{event = []}};

websocket_info(tick, Req, #state{storage=Storage, event=[]} = State) ->
    {ok, Req, State#state{event = handle_ets(Storage)}};


websocket_info(tick, Req, #state{storage=Storage, event=Events} = State) ->
    Result = handle_event_list(Events),
    {reply, {text, Result}, Req, State#state{event=[]}};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{storage=Storage}) ->
    ets:delete(Storage),
    ok.

handle_ets(Storage)->
    First = ets:first(Storage),
    handle_ets(Storage, First, []).

handle_ets(Storage, '$end_of_table', Acc)->
    Acc;

handle_ets(Storage, Key, Acc)->
    Next = ets:next(Storage, Key),
    Event = ets:lookup(Storage, Key),
    Res = handle_event(Event),
    ets:delete(Storage, Key),
    handle_ets(Storage, Next, [Res|Acc]).


handle_event([{_sender, #evman_note{
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
    }}]) when Error =/=  ?EVMAN_UNDEFINED->
    erlang:list_to_binary(io_lib:format(
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
        [   Datetime,
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Error
        ]
    ));

handle_event([{_sender, #evman_note{
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
    }}]) when Warning =/=  ?EVMAN_UNDEFINED->
    erlang:list_to_binary(io_lib:format(
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
        [   Datetime,
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Warning
        ]
    ));

handle_event([{_sender, #evman_note{
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
    }}]) when Notice =/=  ?EVMAN_UNDEFINED->
    erlang:list_to_binary(io_lib:format(
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
        [   Datetime,
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Notice
        ]
    ));

handle_event([{_sender, #evman_note{
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
    }}]) when Info =/=  ?EVMAN_UNDEFINED->
    erlang:list_to_binary(io_lib:format(
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
        [   Datetime,
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Info
        ]
    ));

handle_event([{_sender, #evman_note{
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
    }}]) when Debug =/=  ?EVMAN_UNDEFINED->
    erlang:list_to_binary(io_lib:format(
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
        [   Datetime,
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment,
            Debug
        ]
    ));


handle_event([{_sender, #evman_note{
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
    }}]) when Args =/=  ?EVMAN_UNDEFINED->
    erlang:list_to_binary(io_lib:format(
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
        [   Datetime,
            Line,
            Module,
            Function,
            Arity,
            Args,
            Comment     ]
    ));


handle_event([{_, #evman_note{
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
    } =Event}]) ->
    erlang:list_to_binary(io_lib:format(
        "<hr/><div><pre><code>"
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
        [   Line,
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
    ));

handle_event([{_, #evman_note{
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
    } =Event}]) ->
    erlang:list_to_binary(io_lib:format(
        "<hr/><div><pre><code>"
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
        [   Line,
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
    ));
  
handle_event([{_, Event}])->
    erlang:list_to_binary(io_lib:format("<p> ~p </p>", [Event]));
    
handle_event(Event)->
    erlang:list_to_binary(io_lib:format("<p> ~p </p>", [Event])).

handle_event_list(Events)->
    erlang:list_to_binary(Events).
    

handle_event({_sender, Event}, #state{storage=Storage} = State) ->
    ets:insert_new(Storage, {now(), Event}),
    {ok, State#state{event = Event}};

handle_event(Event,    #state{storage=Storage} = State) ->
    ets:insert_new(Storage, {now(), Event}),
    {ok, State#state{event = Event}}.

handle_call(_request,   State) ->
  {ok, ok, State}.

handle_info(_info,      State) ->
  {ok, State}.

code_change(_OldVsn,    State, _Extra) ->
  {ok, State}.

