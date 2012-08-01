-module(evman_customer).
-behaviour(evman_notifier).

-include("evman.hrl").
-define(EVENTNAME, ?MODULE).


%%% 
%%% evman_notifier API
%%% 
-export([
    start_link/0,
    start_link/1,
    add_handler/2,
    add_guarded_handler/2,
    rem_handler/1,
    rem_guarded_handler/1,
    get_handlers/0,
    info/1
]).


-export([
    create/1,
    delete/1,
    change/1,
    change_perm/1,
    add_perm/1,
    del_perm/1,
    insider/2
]).


%%% -----------------------------------------------------------------------
%%% evman_notifier API
%%% -----------------------------------------------------------------------

start_link() ->
    evman_notifier:start_link(?EVENTNAME).

start_link(Args) ->
    evman_notifier:start_link(?EVENTNAME, Args).

add_handler(ModuleName, Args) ->
    evman_notifier:add_handler(?EVENTNAME, ModuleName, []).

add_guarded_handler(ModuleName, Args) ->
    evman_notifier:add_guarded_handler(?EVENTNAME, ModuleName, Args).

rem_handler(ModuleName) ->
    evman_notifier:rem_handler(?EVENTNAME, ModuleName).

rem_guarded_handler(ModuleName) ->
    evman_notifier:rem_guarded_handler(?EVENTNAME, ModuleName).

get_handlers() ->
    evman_notifier:get_handlers(?EVENTNAME).

info(Msg) ->
    evman_notifier:info(?EVENTNAME, Msg).

%%% -----------------------------------------------------------------------
%%% API
%%% -----------------------------------------------------------------------


%%%
%%% @doc
%%%     Сообщает о создании состояния
%%%
create(Msg) ->
    info({create, Msg}).

%%%
%%% @doc
%%%     Сообщает об удалении аккаунта
%%%
delete(Msg) ->
    info({delete, Msg}).

%%%
%%% @doc
%%%     Сообщает об изменении состояния
%%%
change(Msg) ->
    info({change, Msg}).

%%%
%%% @doc
%%%     Сообщает об изменении прав
%%%
change_perm(Msg) ->
    change({perm, Msg}).

%%%
%%% @doc
%%%     Сообщает о добавлении прав
%%%
add_perm(Msg) ->
    change_perm({add, Msg}).

%%%
%%% @doc
%%%     Сообщает о удалени прав
%%%
del_perm(Msg) ->
    change_perm({del, Msg}).

%%%
%%% @doc
%%%     Сообщает о том что пользователь стал авторизованным
%%%
insider(true, Msg) ->
    add_perm({insider, Msg});

%%%
%%% @doc
%%%     Сообщает о том что пользователь перестал быть авторизованным
%%%
insider(false, Msg) ->
    del_perm({insider, Msg});


insider(Type, Msg) ->
    ?debug("Error in ~p Type = ~p: Msg = ~p", [?EVENTNAME, Type, Msg]).






