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
  get_events/1,
  get_state/0,
  start/0,
  start/1,
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
    level = all,
    events = []
}).



start() ->
    evman:add_sup_handler(?HANDLERNAME, []).

start(Options) ->
    Level = proplists:get_value(level, Options, all),
    evman:add_sup_handler(?HANDLERNAME, [#state{level=Level}]).

get_events() ->
    gen_event:call(?EVMAN, ?HANDLERNAME, get_events).

get_events(Level) ->
    gen_event:call(?EVMAN, ?HANDLERNAME, {get_events, Level}).

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
    {ok, State#state{events = [{utn(), Event}|Events]}};

handle_event(Event,    #state{events=Events} = State) ->
    {ok, State#state{events = [{utn(), Event}|Events]}}.

handle_call(get_state,   State) ->
    {ok, State, State};

handle_call(get_events,   #state{events = Events} = State) ->
    Events1 = lists:sort(
        fun
            (   #evman_note{datetime={Mega1, _, _}},
                #evman_note{datetime={Mega2, _, _}}
            ) when Mega1 > Mega2 ->
                false;
            (   #evman_note{datetime={_, Sec1, _}},
                #evman_note{datetime={_, Sec2, _}}
            ) when Sec1 > Sec2 ->
                false;
            (   #evman_note{datetime={_, _, Micro1}},
                #evman_note{datetime={_, _, Micro2}}
            ) when Micro1 > Micro2 ->
                false;
            (_, _) ->
                true
        end,
        Events
    ),
    {ok, Events1, State#state{events = []}};

handle_call(_request,   State) ->
    {ok, ok, State}.

handle_info(_info,      State) ->
    {ok, State}.

code_change(_OldVsn,    State, _Extra) ->
    {ok, State}.

terminate(_Req, _State) ->
    ok.


utn() ->
    Bas = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Cur = calendar:datetime_to_gregorian_seconds({date(), time()}),
    trunc((Cur - Bas)).