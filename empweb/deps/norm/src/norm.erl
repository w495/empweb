%%
%% @file norm.erl
%%
%%     Модуль преобразования строковых данных
%%         к определенным типам.
%%
%%     Сами типы описаны в отдельно модуле (см ?NORM_CONVERTER).
%%     Это сделано с целью разделить
%%         обязанности между модулями.
%%
%%

-module(norm).

-vsn(1.2).
-authors([cff, w495]).


-export([
    norm/2,
    test/0,
    test/1
]).

-include("norm.hrl").

norm({Data}, Type_rules) ->
    norm(Data, Type_rules);

norm(Data, Type_rules)
        when erlang:is_list(Data)
            andalso erlang:is_list(Type_rules) ->
    norm(Data, Type_rules, #norm{}).

norm(_Data, [], Norm) ->
   Norm;

norm(Data, [#norm_rule{key=Key, nkey=?UNIQ_UNDEFINED}=Rule|Rest], Norm) ->
    norm(Data, [Rule#norm_rule{nkey=Key}|Rest], Norm);

norm(Data, [#norm_rule{key=Rkey, nkey=Nkey, types=Types, required=Required, default=Default}=Rule|Rest], Norm) ->
    Key = case proplists:is_defined(Rkey, Data) of
        true -> Rkey;
        _ -> norm_convert:to_binary(Rkey)
    end,
    case proplists:get_value(Key, Data, Default) of
        ?UNIQ_UNDEFINED ->
            case Required of
                true ->
                    norm(Data, Rest,
                        Norm#norm{
                            errors=[
                                #norm_error{
                                    reason  =   param,
                                    rule    =   Rule
                                } |Norm#norm.errors
                            ]
                        }
                    );
                _ ->
                    norm(Data, Rest, Norm)
            end;
        Raw_value ->
            case to_rule_type(Raw_value, Types) of
                {ok, Value} ->
                    norm(Data,Rest,
                        Norm#norm{
                            return=[
                                {norm_convert:to_atom(Nkey), Value}
                                | Norm#norm.return
                            ]
                        }
                    );
                {predefined, _} ->
                    norm(Data,Rest,
                        Norm#norm{
                            return=[
                                {norm_convert:to_atom(Nkey), Default}
                                | Norm#norm.return
                            ]
                        }
                    );
                {error, Value} ->
                    norm(Data,Rest,
                        Norm#norm{errors=[
                            #norm_error{
                                reason  =   types,
                                value   =   Value, 
                                rule    =   Rule
                            } |Norm#norm.errors
                        ]}
                    )
            end
    end.

%%
%% @doc
%%
%%     Преобразует значение Value к указанным типам.
%%     Обертка функции to_rule_type/3
%%
to_rule_type(Value, Type_rules) ->
    to_rule_type(?NORM_CONVERTER, Value, Type_rules).

rule_type_done(Converter, Value, Type)
        when erlang:is_atom(Type)->
    {ok, Converter:Type(Value)};

rule_type_done(_converter, _value, Type)
        when erlang:is_function(Type, 0) ->
    {ok, Type()};
    
rule_type_done(_converter, Value, Type)
        when erlang:is_function(Type, 1) ->
    {ok, Type(Value)}.

%%
%% @doc
%%
%%     Преобразует значение Value к указанным типам.
%%         Для работы нужно указать модуль Converter,
%%             где должны быть описаны правила преобразования.
%%
to_rule_type(_converter, Value, predefined) ->
    {predefined, Value};

to_rule_type(_converter, Value, any) ->
    {ok, Value};
    
to_rule_type(_converter, Value, []) ->
    {error, Value};

to_rule_type(_converter, Value, [[]|_rest]) ->
    {ok, Value};

to_rule_type(Converter, Value, [Type|Rest]) ->
    try
        rule_type_done(Converter, Value, Type)
    catch
        throw : {type_error, Error} ->
            throw({type_error, Error});
        _t : _e ->
            to_rule_type(Converter, Value, Rest)
    end.

%%
%% @doc
%%
%%     Преобразует значение Value к указанным типам.
%%         Для работы нужно указать модуль Converter,
%%             где должны быть описаны правила преобразования.
%%
test() ->
    io:format("X = ~p~n", [
        norm:norm(
            [{<<"x">>, <<"12">>}],
            [#norm_rule{key=x, types=[integer]}]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [
                {<<"x">>, <<"12">>},
                {<<"y">>, <<"12.0">>}
            ],
            [
                #norm_rule{key=x, types=[integer]},
                #norm_rule{key=y, types=[float]}
            ]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [{<<"y">>, <<"12">>}],
            [#norm_rule{key=x, types=[integer]}]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [
                {<<"y">>, <<"12">>},
                {<<"z">>, <<"12">>}
            ],
            [#norm_rule{key=x, types=[integer]}]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [
             {<<"x">>, <<"12">>}
            ],
            [
                #norm_rule{key=a, types=[integer]},
                #norm_rule{key=b, types=[float]}
            ]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [{<<"x">>, <<"as">>}],
            [#norm_rule{key=x, types=[integer]}]
        )
    ]),
    io:format("X = ~p~n", [
        norm:norm(
            [
                {<<"x">>, <<"12">>},
                {<<"y">>, <<"sdsd">>}
            ],
            [
                #norm_rule{key=x, types=[integer]},
                #norm_rule{key=y, types=[float]}
            ]
        )
    ]),
    ok.

test(speed) ->
    ok.