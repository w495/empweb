-module(norm_types).

-export([
    any/1,
    nullable/1,
    boolean/1,
    atom/1,
    string/1,
    text/1,
    list/1,
    email/1,
    float/1,
    integer/1,
    date_unixtime/1,
    datetime_unixtime/1,
    test/0,
    test/1
]).

nullable(<<"">>) ->
    null;
nullable(<<"nil">>) ->
    null;
nullable(<<"null">>) ->
    null.

boolean(<<"true">>) ->
    true;
boolean(<<"false">>) ->
    false.

atom(Value) ->
    convert:to_atom(Value).

integer(Value) ->
    convert:to_integer(Value).

float(Value) ->
    convert:to_float(Value).

date_unixtime(Value) ->
    {D, _} = convert:to_local_datetime(convert:to_integer(Value)),
    D.

datetime_unixtime(Value) ->
    convert:to_local_datetime(convert:to_integer(Value)).

%% Email проверка
email(Value) ->
    case re:run(Value, expression_email()) of
        {match, _} ->
            Value;
        nomatch ->
            throw({
                type_error,
                {
                    email,
                    [
                        {value, Value}
                    ]
                }
            })
    end.

string(Value) ->
    Value.

text(Value) ->
    Value.

list(Value) ->
    convert:to_list(Value).

any(Value) ->
    Value.


expression_email()->
    {ok, Re} =
        re:compile(
            "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]+"
        ),
    Re.


test() ->
    ok.

test(speed) ->
    ok.