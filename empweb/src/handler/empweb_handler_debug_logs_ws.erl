%% Feel free to use, reuse and abuse the code in this file.

-module(empweb_handler_debug_logs_ws).
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
    level,
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
    {Host, Req} = cowboy_http_req:raw_host(Req),
    {Port, Req} = cowboy_http_req:port(Req),
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],[], Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, _State) ->
    %{Level, Req} = cowboy_req:qs_val(<<"level">>, Req_),

    ?debug("(3)~n"),
    timer:send_interval(1000, tick),
    Req2 = cowboy_http_req:compact(Req),
    ?debug("(4)~n"),
    State = #state{
        storage=ets:new(?HANDLERNAME, [set, public, {write_concurrency,true}])
    },
    ?debug("(4)~n"),
    evman:add_handler(?HANDLERNAME, [State]),
    ?debug("(4)~n"),
    {ok, Req2, State}.

websocket_handle({text, Msg}, Req, State) ->
    ?debug("--> ~p <-- ~n", [?LINE]),
    {reply, {text, base64:encode(<< "You said: ", Msg/binary >>)}, Req, State#state{level=erlang:binary_to_atom(Msg, utf8)}};

websocket_handle(_Any, Req, State) ->
    ?debug("--> ~p <-- ~n", [?LINE]),
    {ok, Req, State}.


websocket_info(tick, Req, #state{event=undefined} = State) ->
    ?debug("--> ~p <-- ~n", [?LINE]),
    {ok, Req, State#state{event = []}};

websocket_info(tick, Req, #state{storage=Storage, level= Level, event=[]} = State) ->
    ?debug("--> ~p <-- Level = ~p ~n", [?LINE, Level]),
    {ok, Req, State#state{event = handle_ets(Storage, State)}};

websocket_info(tick, Req, #state{storage=Storage, event=Events} = State) ->
    ?debug("--> ~p <-- ~n", [?LINE]),
    Result = handle_event_list(Events),
    {reply, {text, Result}, Req, State#state{event=[]}};

websocket_info(_Info, Req, #state{level=Level} = State) ->
    ?debug("--> ~p <-- Level = ~p ~n", [?LINE, Level]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{storage=Storage}) ->
    ?debug("--> ~p <-- ~n", [?LINE]),
    ets:delete(Storage),
    ok.

handle_ets(Storage, State)->
    First = ets:first(Storage),
    handle_ets(Storage, First, [], State).

handle_ets(Storage, '$end_of_table', Acc, State)->
    Acc;

handle_ets(Storage, Key, Acc, State)->
    Next = ets:next(Storage, Key),
    Event = ets:lookup(Storage, Key),
    Res = handle_event1(Event, State),
    ets:delete(Storage, Key),
    handle_ets(Storage, Next, [Res|Acc], State).


handle_event1([{_sender, #evman_note{
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
    }}], State) when Error =/=  ?EVMAN_UNDEFINED->
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

handle_event1([{_sender, #evman_note{
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
    }}], State) when Warning =/=  ?EVMAN_UNDEFINED->
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

handle_event1([{_sender, #evman_note{
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
    }}], State) when Notice =/=  ?EVMAN_UNDEFINED->
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

handle_event1([{_sender, #evman_note{
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
    }}], State) when Info =/=  ?EVMAN_UNDEFINED->
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

handle_event1([{_sender, #evman_note{
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
    }}], State) when Debug =/=  ?EVMAN_UNDEFINED->
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


handle_event1([{_sender, #evman_note{
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
    }}], State) when Args =/=  ?EVMAN_UNDEFINED->
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


handle_event1([{_, #evman_note{
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
    } =Event}], State) ->
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

handle_event1([{_, #evman_note{
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
    } =Event}], State) ->
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
  
handle_event1([{_, Event}],State)->
    erlang:list_to_binary(io_lib:format("<p> ~p </p>", [Event]));
    
handle_event1(Event, State)->
    erlang:list_to_binary(io_lib:format("<p> ~p </p>", [Event])).

handle_event_list([])->
    [];
    %erlang:list_to_binary(
    
handle_event_list([E|_]=Events)->
    Events1 = lists:map(
        fun(X) ->
            base64:encode(X)
            %urlencode(X, [{noplus, true}])
        end,
        Events
    ),
    try
        erlang:list_to_binary(Events1)
    catch
        X:Y ->
            io:format("~p:~p:~p:~p~n", [?MODULE, ?LINE, X, Y])
    end.



urldecode(Bin) when is_binary(Bin) ->
    urldecode(Bin, <<>>, crash).


urldecode(Bin, OnError) when is_binary(Bin) ->
    urldecode(Bin, <<>>, OnError).


urldecode(<<$%, H, L, Rest/binary>>, Acc, OnError) ->
    G = unhex(H),
    M = unhex(L),
    if  G =:= error; M =:= error ->
        case OnError of skip -> ok; crash -> erlang:error(badarg) end,
        urldecode(<<H, L, Rest/binary>>, <<Acc/binary, $%>>, OnError);
        true ->
        urldecode(Rest, <<Acc/binary, (G bsl 4 bor M)>>, OnError)
    end;
urldecode(<<$%, Rest/binary>>, Acc, OnError) ->
    case OnError of skip -> ok; crash -> erlang:error(badarg) end,
    urldecode(Rest, <<Acc/binary, $%>>, OnError);
urldecode(<<$+, Rest/binary>>, Acc, OnError) ->
    urldecode(Rest, <<Acc/binary, $ >>, OnError);
urldecode(<<C, Rest/binary>>, Acc, OnError) ->
    urldecode(Rest, <<Acc/binary, C>>, OnError);
urldecode(<<>>, Acc, _OnError) ->
    Acc.

unhex(C) when C >= $0, C =< $9 -> C - $0;
unhex(C) when C >= $A, C =< $F -> C - $A + 10;
unhex(C) when C >= $a, C =< $f -> C - $a + 10;
unhex(_) -> error.


%% @doc URL encode a string binary.
%% @equiv urlencode(Bin, [])
-spec urlencode(binary()) -> binary().
urlencode(Bin) ->
    urlencode(Bin, []).

%% @doc URL encode a string binary.
%% The `noplus' option disables the default behaviour of quoting space
%% characters, `\s', as `+'. The `upper' option overrides the default behaviour
%% of writing hex numbers using lowecase letters to using uppercase letters
%% instead.

urlencode(Bin, Opts) ->
    Plus = not proplists:get_value(noplus, Opts, false),
    Upper = proplists:get_value(upper, Opts, false),
    urlencode(Bin, <<>>, Plus, Upper).

urlencode(<<C, Rest/binary>>, Acc, P=Plus, U=Upper) ->
    if  C >= $0, C =< $9 -> urlencode(Rest, <<Acc/binary, C>>, P, U);
        C >= $A, C =< $Z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
        C >= $a, C =< $z -> urlencode(Rest, <<Acc/binary, C>>, P, U);
        C =:= $.; C =:= $-; C =:= $~; C =:= $_ ->
        urlencode(Rest, <<Acc/binary, C>>, P, U);
        C =:= $ , Plus ->
        urlencode(Rest, <<Acc/binary, $+>>, P, U);
        true ->
        H = C band 16#F0 bsr 4, L = C band 16#0F,
        H1 = if Upper -> tohexu(H); true -> tohexl(H) end,
        L1 = if Upper -> tohexu(L); true -> tohexl(L) end,
        urlencode(Rest, <<Acc/binary, $%, H1, L1>>, P, U)
    end;
urlencode(<<>>, Acc, _Plus, _Upper) ->
    Acc.

-spec tohexu(byte()) -> byte().
tohexu(C) when C < 10 -> $0 + C;
tohexu(C) when C < 17 -> $A + C - 10.

-spec tohexl(byte()) -> byte().
tohexl(C) when C < 10 -> $0 + C;
tohexl(C) when C < 17 -> $a + C - 10.

-spec x_www_form_urlencoded(binary(), fun((binary()) -> binary())) ->
        list({binary(), binary() | true}).
x_www_form_urlencoded(<<>>, _URLDecode) ->
    [];
x_www_form_urlencoded(Qs, URLDecode) ->
    Tokens = binary:split(Qs, <<"&">>, [global, trim]),
    [case binary:split(Token, <<"=">>) of
        [Token] -> {URLDecode(Token), true};
        [Name, Value] -> {URLDecode(Name), URLDecode(Value)}
    end || Token <- Tokens].

    
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

