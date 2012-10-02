%%%
%%% @file storage.erl
%%%
%%%     Некотрое ОЗУ хранилище.
%%%     Нужно, чтобы отделиться от реализации ets
%%%     Это может быть полезно, если мы с ets перейдем на что-то иное,
%%%         например на gb_set или gb_tree,
%%%         или захотим инкапсулировать каждый словарь в gen_server.
%%%
%%%     Кроме того, на базе этого модуля можно организовать простое
%%%         и быстрое кеширование с вытесненнием параметров
%%%             (как это сделано для капчи).
%%%     Сечас это просто обертка над ets.
%%%

-module(storage).

-export([
    new/1,
    newc/1,
    value/2,
    lookup/2,
    insert/3,
    delete/2,
    delete/1,
    delete_all_objects/1,
    select/2,
    select/3,
    remove_expired/1,
    timeout/1,
    remove_expired/0,
    timeout/0,
    test/0,
    test/1
]).

%%% ----------------------------------------------------------------------
%%% API
%%% ----------------------------------------------------------------------


new(Name)->
    internal_new(Name, []).

newc(Name)->
    internal_newc(Name, []).

insert(Name, Key, Value) ->
    internal_insert(Name, Key, Value).

value(Name, Key) ->
    internal_lookup_value(Name, Key).

lookup(Name, Key) ->
    internal_lookup(Name, Key).

delete(Name, Key) ->
    internal_delete(Name, Key).

delete(Name) ->
    internal_delete(Name).

delete_all_objects(Name) ->
    internal_delete_all_objects(Name).

%%%
%%% @doc
%%%     Нежелательная опирация, так она оставляет зависимость от ets
%%%
select(Name, Match_spec) ->
    internal_select(Name, Match_spec).

select(Name, Match_spec, Limit) ->
    internal_select(Name, Match_spec, Limit).

remove_expired(Name)->
    %delete_all_objects(Name),
    ok.

timeout(Name)->
    %delete_all_objects(Name),
    ok.

remove_expired()->
    ok.

timeout()->
    ok.

test()->
    ok.

test(speed)->
    ok.

%%% ----------------------------------------------------------------------
%%% Внутренние функции
%%% ----------------------------------------------------------------------

internal_newc(Name) ->
    internal_newc(Name, []).

internal_newc(Name, Opts) ->
    internal_new(Name, [{write_concurrency,true}|Opts]).

internal_new(Name) ->
    internal_new(Name, []).

internal_new(Name, Opts) ->
    case ets:info(Name) of
        undefined -> ets:new(Name, [set, public, named_table|Opts]);
        _         -> Name
    end.

internal_insert(Name, Key, Value) ->
    case ets:insert(Name, {Key, Value}) of
        true ->
            true;
        Else ->
            Else
    end.

internal_lookup(Name, Key) ->
    case ets:lookup(Name, Key) of
        [{Key, Value}] ->
            {Key, Value};
        [] ->
            undefined;
        Else ->
            Else
    end.

internal_lookup_value(Name, Key) ->
    case ets:lookup(Name, Key) of
        [{Key, Value}] ->
            Value;
        [] ->
            undefined;
        Else ->
            Else
    end.


internal_delete(Name, Key) ->
    ets:delete(Name, Key).

internal_delete(Name) ->
    ets:delete(Name).

internal_delete_all_objects(Name) ->
    ets:delete_all_objects(Name).

internal_select(Name, Ets_match_spec) ->
    ets:select(Name, Ets_match_spec).

internal_select(Name, Ets_match_spec, Limit) ->
    ets:select(Name, Ets_match_spec, Limit).

