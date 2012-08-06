%% Feel free to use, reuse and abuse the code in this file.

-module(handler_websocket).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-behaviour(gen_event).
-compile({parse_transform, lager_transform}).

-include("empweb.hrl").

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


init({_Any, http}, Req, []) ->    
    case cowboy_http_req:header('Upgrade', Req) of
        {undefined, Req2} -> {ok, Req2, req};
        {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->

%     {ok, App} = application:get_application(),
%     {ok, Http} = application:get_env(App, http),
%     Port =proplists:get_value(port, Http),

    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],

%% HTML code taken from misultin's example file.
<<"<html>
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
        =   \"<p>\" + date + \" server :<p><tt> \" + text + \"</tt></p></p>\"
            + document.getElementById('status').innerHTML;
}
function ready(){
    if (\"MozWebSocket\" in window) {
        WebSocket = MozWebSocket;
    }
    if (\"WebSocket\" in window) {
        // browser supports websockets
        var ws = new WebSocket(\"ws://localhost:8000/websocket\");
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
</html>">>, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, _State) ->
    timer:send_interval(1000, tick),
    Req2 = cowboy_http_req:compact(Req),
    State = #state{storage=ets:new(?HANDLERNAME, [set, public, {write_concurrency,true}])},
    evman:add_handler(?HANDLERNAME, [State]),
    ?debug("1~n"),
    {ok, Req2, State}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "You said: ", Msg/binary >>}, Req, State};

websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.


websocket_info(tick, Req, #state{event=undefined} = State) ->
    {ok, Req, State#state{event = []}};

websocket_info(tick, Req, #state{storage=Storage, event=[]} = State) ->
    Events = handle_ets(Storage),
    {ok, Req, State#state{event = Events}};


websocket_info(tick, Req, #state{storage=Storage, event=Events} = State) ->
    Result = erlang:list_to_binary(Events),
    {reply, {text, Result}, Req, State#state{event=[]}};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{storage=Storage}) ->
    ?debug("---------------------------------------------------~n"),
    ets:delete(Storage),
    ok.

handle_ets(Storage)->
    Elist =
        case ets:first(Storage) of
            '$end_of_table'   -> [];
            First_key -> ets:lookup(Storage, First_key)
        end,
    lisrs:map(
        fun(Event)->
            Result = handle_event(Event),
            ets:delete_object(Storage, Event),
            Result
        end,
        Elist
    ).

handle_event(Event)->
    erlang:list_to_binary(io_lib:format("~p", [Event])).


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

