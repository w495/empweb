%% Feel free to use, reuse and abuse the code in this file.

-module(handler_weblog).
-behaviour(cowboy_http_handler).
-behaviour(gen_event).
-compile({parse_transform, lager_transform}).

-include("empweb.hrl").

-export([
    init/3,
    handle/2,
    terminate/2
]).

-export([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).


-define(HANDLERNAME, ?MODULE).

-record(state, {
    req
}).



init([]) ->
    {ok, #state{}};

init([State]) ->
    {ok, State}.

init({_Any, http}, Req, []) ->
    State = #state{req = Req},
    evman:add_handler(?HANDLERNAME, [State]),
    {loop, Req, State}.


handle(Req, State) ->
    {ok, Resreq} = cowboy_http_req:chunked_reply(200, Req),

    ok = cowboy_http_req:chunk("log \r\n", Resreq),

    {ok, Resreq, State}.

terminate(_reason, #state{req = Req} = State) ->
  %?debug("~p ~p was terminate with reason `~p' and state `~p' ~n",
  %  [?HANDLERNAME, self(), _reason, _state]),
  ok;

terminate(_Req, _State) ->
    ok.


handle_event({_sender, Eevent}, #state{req = Req} = State) ->
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

handle_call(_request, #state{req = Req} = State) ->
  %?debug("~p ~p has call `~p' and state `~p' ~n",
  %  [?HANDLERNAME, self(), _request, State]),
  {ok, ok, State}.

handle_info(_info, #state{req = Req} = State) ->
  %   ?debug("~p ~p has info `~p' and state `~p' ~n",
  %     [?HANDLERNAME, self(), _info, State]),
  {ok, State}.



code_change(_OldVsn, #state{req = Req} = State, _Extra) ->
  {ok, State}.




