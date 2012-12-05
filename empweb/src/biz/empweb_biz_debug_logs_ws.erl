%% Feel free to use, reuse and abuse the code in this file.

-module(empweb_biz_debug_logs_ws).
-behaviour(gen_event).


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
  get_events/0,
  get_state/0,
  start/0,
  stop/0
]).

-export([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  code_change/3,
  terminate/2
]).


-define(HANDLERNAME, ?MODULE).
-define(EVMAN, evman).


-record(state, {
    level,
    event,
    events
}).



start() ->
    evman:add_sup_handler(?HANDLERNAME, []).

get_events() ->
    gen_event:call(?EVMAN, ?HANDLERNAME, get_events).

get_state() ->
    gen_event:call(?EVMAN, ?HANDLERNAME, get_state).

stop() ->
    evman:delete_sup_handler(?HANDLERNAME, []).


init([]) ->
    State = #state{},
    init([State]);

init([State]) ->
    {ok, State}.


handle_event({_sender, Event}, #state{events=Events} = State) ->
    {ok, State#state{events = [{now(), Event}|Events]}};

handle_event(Event,    #state{events=Events} = State) ->
    {ok, State#state{events = [{now(), Event}|Events]}}.

handle_call(get_state,   State) ->
    {ok, State, State};

handle_call(get_events,   #state{events = Events} = State) ->
    {ok, Events, State#state{events = []}};

handle_call(_request,   State) ->
    {ok, ok, State}.

handle_info(_info,      State) ->
    {ok, State}.

code_change(_OldVsn,    State, _Extra) ->
    {ok, State}.

terminate(_Req, _State) ->
    ok.

