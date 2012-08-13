-module(dao).


-include_lib("epgsql/include/pgsql.hrl").

-compile(export_all).

-define(SVAR, "$").

%%%
%%% Спецификации
%%%

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {table,     0},
            %% ::= table(name)
        {table,     1},
            %% {fields, insert, required}
            %% {fields, insert}
            %% {fields, select}
            %% {fields, update}
            %% {fields, all}
            %% name
        {create,    2},
        {update,    2},
        {get,       2},
        {get,       3},
        {is_owner,  2}
    ];
behaviour_info(_Other) ->
    undefined.


%%% -----------------------------------------------------------------------

fields(all) ->
    fields([], all);

fields(Fields) ->
    fields(Fields, []).

fields(Fields, all) ->
    fields(Fields, <<"*">>);

fields(Fields, Default) ->
    fields(Fields, Default, []).

fields(all, Default, Additions) ->
    fields_([], [], Default, Additions);

fields(Fields, Default, Additions) ->
    fields_([], Fields, Default, Additions).
    
%%% -----------------------------------------------------------------------

table_fields(Table, Fields) ->
    table_fields(Table, Fields, []).

table_fields(Table, Fields, Default) ->
    table_fields(Table, Fields, Default, []).

table_fields(Table, Fields, Default, Additions) ->
    fields_([convert:to_binary(Table), <<".">>], Fields, Default, Additions).

%%% -----------------------------------------------------------------------

fields_(_, [], [], _additions) ->
   <<>>;

fields_(Table, [], [D|Rest] = Default, Additions) when erlang:is_atom(D) ->
    fields_(Table, Default, [], Additions);

fields_(_, [], Default, _additions) ->
   Default;

fields_(Table, Fields, _default, Additions) ->
    [[_|First]|Res] = lists:map(fun(Field)->
        [<<",">> ,[convert:to_binary(Table), convert:to_binary(Field)]]
    end,lists:append(Fields, Additions)),
    [<<" ">>, [First|Res], <<" ">>].

%%% -----------------------------------------------------------------------


fieldvars(Fields) ->
    fieldvars(Fields, []).

fieldvars(Fields, Default) ->
    fieldvars(Fields, Default, []).

fieldvars([], Default, _additions) ->
    Default;
fieldvars(Fields, _default, Additions) ->
    [[_|First]|Res] = lists:map(fun(Field)->
        [<<",">> , [<<"$">>, convert:to_binary(Field)]]
    end,lists:append(Fields, Additions)),
    [<<" ">>, [First|Res], <<" ">>].

%%% -----------------------------------------------------------------------

fields_fieldvars(Fields)->
    fields_fieldvars(Fields, []).

fields_fieldvars(Fields, Default)->
    fields_fieldvars(Fields, Default, []).

fields_fieldvars([], Default, _additions) ->
    Default;
fields_fieldvars(Fields, _default, Additions) ->
    [[_|First]|Res] = lists:map(fun(Field)->
        [<<",">> , [convert:to_binary(Field), <<"=$">>, convert:to_binary(Field)]]
    end,Fields),
    [<<" ">>, [First|Res], <<" ">>].

%%% -----------------------------------------------------------------------

get({Parent, Fp}, {Module, Fm}, Con, {Key, Value}, Fields)->
    Ap = Parent:table({fields, all}),
    Sp = Parent:table({fields, select}),
    Am = Module:table({fields, all}),
    Sm = Module:table({fields, select}),
    Tfields = lists:filter(fun(F)-> lists:member(F, Sp) or lists:member(F, Sm) end, Fields),
    case lists:member(Key, lists:append(Ap, Am)) of
        true ->
            Ptbl = convert:to_binary(Parent:table(name)),
            Mtbl = convert:to_binary(Module:table(name)),
            Dfs = dao:fields(Tfields, lists:append(Sp, Sm)),
            Ckey = convert:to_binary(Key),
            Fmb = convert:to_binary(Fm),
            Fpb = convert:to_binary(Fp),
            dao:pgret(dao:equery(Con,[
                <<" select ">>, Dfs,    <<" from ">>,   Ptbl,
                <<" join ">>,   Mtbl,   <<" on ">>,
                    [Mtbl,<<".">>,Fmb], <<" =  ">>,  [Ptbl,<<".">>,Fpb],
                <<" where ">>,  Ckey,   <<" = $1 ">>
            ], [Value]));
        _ ->
            {error, {wrong_field, Key}}
    end;

get(Par, Cur, Con, [{Key, Value}], Fields)->
    get(Par, Cur, Con, {Key, Value}, Fields);

get({Parent, Fp}, {Module, Fm}, Con, all, Fields)->
    Ap = Parent:table({fields, all}),
    Sp = Parent:table({fields, select}),
    Am = Module:table({fields, all}),
    Sm = Module:table({fields, select}),
    Tfields = lists:filter(fun(F)-> lists:member(F, Sp) or lists:member(F, Sm) end, Fields),
    Ptbl = convert:to_binary(Parent:table(name)),
    Mtbl = convert:to_binary(Module:table(name)),
    Dfs = dao:fields(Tfields, lists:append(Sp, Sm)),
    Fmb = convert:to_binary(Fm),
    Fpb = convert:to_binary(Fp),
    dao:pgret(dao:equery(Con,[
        <<" select ">>, Dfs,    <<" from ">>,   Ptbl,
        <<" join ">>,   Mtbl,   <<" on ">>,
            [Mtbl,<<".">>,Fmb], <<" =  ">>,  [Ptbl,<<".">>,Fpb]
    ]));

get(Par, Cur, Con, [], Fields)->
    get(Par, Cur, Con, all, Fields).


%%% -----------------------------------------------------------------------

get(Module, Con, {Key, Value}, Fields)->
    Afields = Module:table({fields, all}),
    Sfields = Module:table({fields, select}),
    Tfields = lists:filter(fun(F)-> lists:member(F, Sfields) end, Fields),
    case lists:member(Key, Afields) of
        true ->
            Mtbl = convert:to_binary(Module:table(name)),
            Dfs = dao:fields(Tfields, Sfields),
            Ckey = convert:to_binary(Key),
            dao:pgret(dao:equery(Con,[
                <<"select ">>,Dfs,<<" from ">>,Mtbl,<<" where ">>,Ckey,<<" = $1">>
            ], [Value]));
        _ ->
            {error, {wrong_field, Key}}
    end;

get(Module, Con, [{Key, Value}], Fields)->
    get(Module, Con, {Key, Value}, Fields);

get(Module, Con, all, Fields)->
    Sfields = Module:table({fields, select}),
    Tfields = lists:filter(fun(F)-> lists:member(F, Sfields) end, Fields),
    Mtbl = convert:to_binary(Module:table(name)),
    Dfs = dao:fields(Tfields, Sfields),
    dao:pgret(dao:equery(Con,[<<"select ">>,Dfs,<<" from ">>,Mtbl]));

get(Module, Con, [], Fields)->
    get(Module, Con, all, Fields).

%%% -----------------------------------------------------------------------

create({Parent, Fp}, {Module, Fm}, Con, Proplist, Ret1, Ret2)->
    case create(Parent, Con, Proplist, Ret1) of
        {ok, Pid} ->
            io:format("~nFm = ~p~n", [Fm]),
            io:format("~nPid = ~p~n", [Pid]),
            create(Module, Con, [{Fm, Pid}|Proplist], Ret2);
        {error, Error} ->
            Error
    end.

create({Parent, Fp}, {Module, Fm}, Con, Proplist, Ret2)->
    create({Parent, Fp}, {Module, Fm}, Con, Proplist, id, Ret2).

create(Module, Con, Proplist)->
    create(Module, Con, Proplist, id).

create(Module, Con, Proplist, Ret)->
    io:format("~nProplist = ~p~n", [Proplist]),
    Fields = lists:filter(
        fun(F)-> lists:member(F, Module:table({fields, insert})) end,
        proplists:get_keys(Proplist)
    ),
    Required = Module:table({fields, insert, required}),
    case lists:foldl( fun(F, R)-> R and lists:member(F, Fields) end, true, Required ) of
        true ->
            Mtbl =  convert:to_binary(Module:table(name)),
            Dfs =   dao:fields(Fields),
            Dfvs  = dao:fieldvars(Fields),
            Retb = convert:to_binary(Ret),
            dao:pgret(dao:equery(Con,[
                <<"insert into ">>,
                    Mtbl,
                <<"(">>,
                    Dfs,
                <<")values(">>,
                    Dfvs,
                <<") returning ">>,
                    Retb
            ], Proplist));
        _ ->
            {error, {required, Required}}
    end.

update({Parent, Fp}, {Module, Fm}, Con, Proplist)->
    case update(Parent, Con, Proplist) of
        {ok, Pid} ->
            update(Module, Con, [{Fm, Pid}|Proplist]);
        {error, Error} ->
            Error
    end.


update(Module, Con, Proplist)->
    Fields = lists:filter(
        fun(F)-> lists:member(F, Module:table({fields, update})) end,
        proplists:get_keys(Proplist)
    ),
    case proplists:get_value(id, Proplist) of
        undefined ->
            create(Module, Con, Proplist);
        Id ->
            Mtbl = convert:to_binary(Module:table(name)),
            Dffvs = dao:fields_fieldvars(Fields),
            dao:pgret(dao:equery(Con,[
                <<" update  ">>, Mtbl, <<" set ">>, Dffvs, <<" where id = $id ">>
            ],Proplist)),
            {ok, Id}
    end.


%%% -----------------------------------------------------------------------
%%% -----------------------------------------------------------------------




pg2rs({ok, _, Vals}, Record_name) ->
    [list_to_tuple([Record_name | tuple_to_list(X)]) || X <- Vals];

pg2rs(Vals, Record_name) ->
    [list_to_tuple([Record_name | tuple_to_list(X)]) || X <- Vals].

strip_rs(Vals) when is_list(Vals)->
    strip_rs(Vals, []);

strip_rs(Val) when is_tuple(Val) ->
    [_|T]=tuple_to_list(Val),
    T.

strip_rs([Vh|Vt], Ret) ->
    [_|T]=tuple_to_list(Vh),
    strip_rs(Vt, [list_to_tuple(T)|Ret]);
strip_rs([], Ret) ->
    Ret.

collect_where_params(Params) ->
    collect_where_params([], [], Params).

collect_where_params(Where, Params, [{Val}|T]) when is_list(Val) ->
    collect_where_params([Val|Where], Params, T);
collect_where_params(Where, Params, [{Key, Val}|T]) ->
    collect_where_params(Where, Params, [{Key, "=", Val}|T]);
%    collect_where_params([lists:append([Key, " = $", convert:to_list(length(Params) + 1)])|Where], [Val | Params], T);
collect_where_params(Where, Params, [{Key, Action, Val}|T]) ->
    collect_where_params([lists:append([Key, " ", Action, " $", convert:to_list(length(Params) + 1)])|Where], [Val | Params], T);
collect_where_params(Where, Params, []) when length(Where) > 0->
    {" WHERE", [string:join(lists:reverse(Where), " and ")], lists:reverse(Params)};
collect_where_params(_, _, []) ->
    {";", []}.


to_type(null, _Type) ->
    null;
to_type(V, int4) ->
    convert:to_integer(V);

to_type(V, numeric) ->
    convert:to_integer(V);

to_type(<<"f">>, bool) ->
    false;
to_type(<<"t">>, bool) ->
    true;

to_type(V, varchar) ->
    V;
to_type(V, text) ->
    V;
to_type(V, Type) ->
    V.


% name_columns([{column, Name, Type, _P3, _P4, _P5}|Ct], [V|Vt], Ret) ->
%     name_columns(Ct, Vt, [{binary_to_list(Name), to_type(V, Type)}|Ret]);
    
name_columns([{column, Name, Type, _P3, _P4, _P5}|Ct], [V|Vt], Ret) ->
    name_columns(
        Ct,
        Vt,
        [
            {
                erlang:list_to_atom(
                    erlang:binary_to_list(Name)
                ),
                to_type(V, Type)
            }
            | Ret
        ]
    );
    
name_columns([], [], Ret) ->
    {Ret};
name_columns([], V, Ret) ->
    %?D("unexpected values: ~p~n", [V]),
    {Ret};
name_columns(C, [], Ret) ->
    %?D("unexpected columns: ~p~n", [C]),
    {Ret}.

make_proplist(Columns, [V|T], Ret) ->
    make_proplist(Columns, T, [name_columns(Columns, tuple_to_list(V), [])|Ret]);
make_proplist(_C, [], Ret) ->
    Ret.

pgret(returning, {ok, Id}) ->
    
    {ok, Id};

pgret(returning, {ok, 1, _, [{Value}]}) ->

    {ok, Value};

pgret(_, Value) ->

    pgret(Value).

pgret({return, Value}) ->

    {ok, Value};

%%%
%%% select sq & eq
%%%
pgret({ok, Columns, Vals}) ->
    
    {ok, make_proplist(Columns, Vals, [])};


pgret([{ok, _columns, _vals}|_rest] = List) ->

    pgret_mult(List, []);
    
%%%
%%% update sq & eq
%%%
pgret({ok, _Count}) ->

    ok;

    
%%%
%%% insert sq & eq
%%%
pgret({ok, 1, _, [{Value}]}) ->

    {ok, Value};

%%%
%%% insert sq & eq
%%%
pgret({ok, _Count, _Columns, _Rows}) ->

    ok;

%%% 
%%% @doc    Ошибка сиквела - неожиданный возврат в функции
%%%         дает ошибку ожидаемого возврата.
%%%
pgret({pgcp_error, E}) ->

    pgreterr(E);

pgret({error, E}) ->

    pgreterr(E);

pgret(V) ->

    {retVal, V}.

pgret_mult([], Acc) ->
    Acc;
pgret_mult([{ok, Columns, Vals}|Rest], Acc) ->
    Res = make_proplist(Columns, Vals, []),
    pgret_mult(Rest, [{ok, Res}|Acc]).


pgreterr({error, E}) ->
    pgreterr(E);
pgreterr({badmatch, E}) ->
    pgreterr(E);
pgreterr(#error{code=Error_code_bin, message=Msg}) ->
    case Error_code_bin of
        <<"23502">> ->
            try
                {ok, RE} = re:compile("\"(.+)\""),
                {match, [_, C | _]} = re:run(Msg, RE, [{capture, all, list}]),
                {error, {not_null, erlang:list_to_binary(C)}}
            catch
                E:R ->
                    io:format("pgret ERROR: ~p - ~p~n", [E, R]),
                    {error, {unknown, Msg}}
            end;
        <<"23505">> ->
            try
                {ok, RE} = re:compile("\"(.*?)*?_([^_].+)_key\""),
                {match, [_, _, C | _]} = re:run(Msg, RE, [{capture, all, list}]),
                {error, {not_unique, erlang:list_to_binary(C)}}
            catch
                E:R ->
                    io:format("pgret ERROR: ~p - ~p~n", [E, R]),
                    {error, {unknown, Msg}}
            end;
        _ ->
            {error, {unknown, Msg}}
    end;
pgreterr(E) ->
    {error, {unexpected, E}}.


%%
%% 
%%
with_connection(Function) ->
    with_connection(emp, Function).

with_connection(Pool, Function) ->
    psqlcp:with_connection(Pool, Function).
    
%%
%%
%%
with_transaction(Function) ->
    with_transaction(emp, Function).

with_transaction(Pool, Function) ->
    psqlcp:with_transaction(Pool, Function).

%%%
%%% @doc    Выполняет преобразование ответа базы в соответсвии
%%%         со здравым смыслом. Если запрашиваем 1 объект,
%%%         то и разультат должны быть один. Вот это лочигнее
%%%         {ok, [{"name", "Name"}]} =
%%%             dao:one(
%%%                 dao:simple("select name from table where id = 1")
%%%             ).
%%%         чем
%%%             {ok, [[{"name", "Name"}]]} =
%%%                 dao:simple("select name from table where id = 1").
%%%
one({ok, []})         -> {ok, undefined};
one({ok, [Some]})     -> {ok, Some};
one({ok, Several})    -> {ok, Several};
one(Error)            -> Error.

%%% 
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error}
%%%         Может возвращать результат побочного действия.
%%%         Соединение создается само для этого запроса.
%%%
simple(Query)
    when erlang:is_list(Query)
        %orelse  erlang:is_binary(Query)
        orelse  erlang:is_function(Query)
        orelse  (
                    erlang:is_tuple(Query) andalso
                    2 =:= erlang:size(Query) andalso
                    erlang:is_function(erlang:element(1, Query)) andalso
                    erlang:is_function(erlang:element(2, Query))
                ) ->
    dao:with_connection(fun(Con)->eqret(Con, Query)end).

%%%
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error} | [{ok, Result}] | [{error, Error}]
%%%         Возможен множественный запрос без параментров.
%%%         Запросы должны отделяться через `;'.
%%%         Может возвращать результат побочного действия.
%%%         Соединение создается само для этого запроса.
%%%
simple_bulk(Query)
    when erlang:is_list(Query)
        %orelse  erlang:is_binary(Query)
        orelse  erlang:is_function(Query)
        orelse  (
                    erlang:is_tuple(Query) andalso
                    2 =:= erlang:size(Query) andalso
                    erlang:is_function(erlang:element(1, Query)) andalso
                    erlang:is_function(erlang:element(2, Query))
                ) ->
    dao:with_connection(fun(Con)->sqret(Con,Query)end).


%%% 
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error}
%%%         Может возвращать результат побочного действия.
%%%         Соединение создается само для этого запроса.
%%%
simple(Query, Params) ->
    dao:with_connection(fun(Con)->eqret(Con, Query, Params)end).

%%%
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error} 
%%%         Может возвращать результат побочного действия.
%%%         Обертка для функции ?MODULE:equery/2 -> ?MODULE:pgret/1
%%%         Кроме запроса необходимо еще указать соединение.
%%%
eqret(Con, Query)->
    io:format("1 eqret(Con, Query)->~n"),
    S = pgret(equery(Con, Query)),
    io:format("2 eqret(Con, Query)->~n"),
    S.

%%%
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error} | [{ok, Result}] | [{error, Error}]
%%%         Возможен множественный запрос без параментров.
%%%         Запросы должны отделяться через `;'.
%%%         Может возвращать результат побочного действия.
%%%         Обертка для функции ?MODULE:squery/2 -> ?MODULE:pgret/1
%%%         Кроме запроса необходимо еще указать соединение.
%%%
sqret(Con, Query)->
    pgret(squery(Con, Query)).

%%%
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error}
%%%         Может возвращать результат побочного действия.
%%%         Обертка для функции ?MODULE:еquery/3 -> ?MODULE:pgret/1
%%%         Кроме запроса необходимо еще указать соединение.
%%%
eqret(Con, Query, Params)->
    io:format("1 eqret(Con, Query, Params)->~n"),
    X = pgret(equery(Con, Query, Params)),
    io:format("2 eqret(Con, Query, Params)->~n"),
    X.

% ---------------------------------------------------------------------------
% %%% @depricated
% %%% @doc  Выполняет простой запрос
% %%%       и возвращает результат побочного действия
% %%%
% simple_ret(Query, Params) ->
%     Pre_result = dao:with_connection(
%             fun(Con) -> equery(Con, Query, Params)
%         end),
%     case dao:pgret(Pre_result) of
%         ok ->
%             Pre_result;
%         Error ->
%             {error, Error}
%     end.
% ---------------------------------------------------------------------------

%%% 
%%% @doc    Обертка для функции ?MODULE:equery/3 c функцией запроса
%%%         Выполныет запрос заданный функцией.
%%%         После самого запроса выполняется Callback
%%%         Callback выполняется отдельным потоком,
%%%             но при этом в случае его падения (в случае ошибки),
%%%                 текущий процесс тоже упадет
%%%
equery(Con, {Qfunction, Callback}, Params)
        when erlang:is_function(Qfunction)
            andalso erlang:is_function(Callback) ->
    Result = equery(Con, Qfunction, Params),
    spawn_link(
        ?MODULE,
        fcallback,
        [
            Con,
            Callback,
            {
                Qfunction,
                Params,
                Result
            }
        ]
    ),
    Result;

%%% 
%%% @doc    Обертка для функции ?MODULE:equery/3
%%%         Выполныет запрос заданный функцией.
%%%         Функция обязана возвращать строку.
%%%
equery(Con, Qfunction, Params) when erlang:is_function(Qfunction) ->
    equery(Con, fquery(Con, Qfunction, Params), Params);

%%% 
%%% @doc    Обертка для стандвартной функции psqlcp
%%%         Выполныет заданный запрос
%%%         Если параметры функции являются proplist
%%%         то происходит их распарсивание и подстановка согласно
%%%         с буквенными именами в теле запроса
%%%         Сделано это для того, чтобы в случае, если мы будем менять,
%%%         наши запросы, не приходилось высчитывать порядок следования.
%%%         Последнее очень не удобно для запросов с более чем 5 параметров.
%%%
equery(Con, Query, Params) when erlang:is_list(Query);erlang:is_binary(Query) ->
    case is_proplist(Params) of
        true ->
            {NewQuery, Values} = equery_pl(Query, Params),
            io:format("NewQuery = ~p~n", [NewQuery]),
            psqlcp:equery(Con, NewQuery, Values);
        _ ->
            io:format("Query = ~p~n", [list_to_binary(Query)]),
            psqlcp:equery(Con, Query, Params)
    end.

%%% 
%%% @doc    Обертка для функции ?MODULE:equery/2
%%%         Выполныет запрос заданный функцией.
%%%         Функция обязана возвращать строку.
%%%
equery(Con, {Qfunction, Callback})
        when erlang:is_function(Qfunction)
            andalso erlang:is_function(Callback) ->
    Result = equery(Con, Qfunction),
    spawn_link(
        ?MODULE,
        fcallback,
        [
            Con,
            Callback,
            {
                Qfunction,
                Result
            }
        ]
    ),
    Result;

equery(Con, Function) when erlang:is_function(Function) ->
    equery(Con, fquery(Con, Function, []));

equery(Con, Query) when erlang:is_list(Query); erlang:is_binary(Query) ->
    io:format("equery(Con, Query) when erlang:is_list(Query) "),
    psqlcp:equery(Con, Query).


squery(Con, {Qfunction, Callback})
        when erlang:is_function(Qfunction)
            andalso erlang:is_function(Callback) ->
    Result = squery(Con, Qfunction),
    spawn_link(
        ?MODULE,
        fcallback,
        [
            Con,
            Callback,
            {
                Qfunction,
                Result
            }
        ]
    ),
    Result;

squery(Con, Function) when erlang:is_function(Function) ->
    squery(Con, fquery(Con, Function, []));

squery(Con, Query) when erlang:is_list(Query) ->
    psqlcp:squery(Con, Query).


%%% 
%%% @doc    Функция преобразования запроса с буквенными именами к числовым.
%%%         Запрос: dao:equery_pl(
%%%                     "select id from customer where id = $id and uid = $uid",
%%%                     [{"id", 10}, {"uid", 15}]
%%%                 ).
%%%         Вернет: "select id from customer where id = $1 and uid = $2"
%%%
equery_pl(Query, Proplist) ->
    equery_pl(Query, Proplist, 1, []).

%%%
%%% @doc
%%%     Функция преобразования запроса, основная рабочая часть.
%%%     Параметры разбиваем на {k, v}
%%%         Преобразованное имя ключа $k заменится
%%%             на его порядковый номер в списке.
%%%         Значение (v) отправится в список значений,
%%%             который далее будет использован для стандартного запроса
%%%             тут важно соблюсти порядок следования значений
%%%                 (Values ++[Value]) a не [Value|Values] !!!
%%%     Чтобы не перекомпилевать одинаковые ключи
%%%         результаты подстановки сохраняются
%%%             с использованием мемоизациию.
%%%
equery_pl(Query, [], _, Values) ->
    {Query, Values};
equery_pl(Query, [{Name, Value}|Rest], Cnt, Values) ->
    %%%
    %%% Мемоизациия для вычисления 1 раз.
    %%%
    Newquery = memo:lsave(fun equery_construct/3, [Query, Name, Cnt]),
    case Newquery == Query of
        true ->
            equery_pl(Query, Rest, Cnt, Values);
        _ ->
            equery_pl(Newquery, Rest, Cnt + 1, lists:append(Values, [Value]))
    end;
equery_pl(Query, List, _, _) when erlang:is_list(List) ->
    {Query, List}.

%%%
%%% @doc
%%%     Функция преобразования запроса,
%%%     Подстановка.
%%%
equery_construct(Query, Name, Cnt) ->
    %%% 
    %%% Мемоизациия для вычисления 1 раз.
    %%% Многие паттерны встречаются достаточно часто,
    %%%     более чем в одном запросе
    %%% Например id, name ...
    %%%
    re:replace(
        Query,
        memo:lsave(fun equery_construct_re/1, [Name]),
        ?SVAR ++ convert:to_list(Cnt),
        [global, {return,list}]
    ).

%%%
%%% @doc
%%%     Функция преобразования запроса,
%%%     Формирование паттерна для подстановки
%%%
equery_construct_re(Name)->
    {ok, Cre} =
        re:compile(
            "[" ++
                ?SVAR ++
            "]" ++
            convert:to_list(Name)
        ),
    Cre.

%%% 
%%% @doc
%%%     Конструирует запрос, заданный функцией.
%%%     Кеширует резльтат выполнения функции.
%%%     !ВАЖНО: что функция вызывается до выполнения самого запроса
%%%
fquery(Con, Qfunction, Param)
        when erlang:is_function(Qfunction, 2) ->
    memo:lsave(Qfunction, [Con, Param]);

fquery(Con, Qfunction, _)
        when erlang:is_function(Qfunction, 1) ->
    memo:lsave(Qfunction, [Con]);

fquery(_, Qfunction, _)
        when erlang:is_function(Qfunction, 0) ->
    memo:lsave(Qfunction, []).


%%% @doc
%%%     Конструирует запрос, заданный функцией.
%%%     Кеширует резльтат выполнения функции.
%%%     !ВАЖНО: что функция вызывается после выполнения самого запроса
%%%
fcallback(Con, Callback, {Qfunction, Params, Result})
        when erlang:is_function(Callback, 4) ->
    memo:lsave(Callback, [Con, Qfunction, Params, Result]);

fcallback(Con, Callback, {Qfunction, Params, Result})
        when erlang:is_function(Callback, 2) ->
    memo:lsave(Callback, [Con, {Qfunction, Params, Result}]);

fcallback(Con, Callback, _)
        when erlang:is_function(Callback, 1) ->
    memo:lsave(Callback, [Con]);

fcallback(_, Callback, _)
        when erlang:is_function(Callback, 0) ->
    memo:lsave(Callback, []).

%%% -------------------------------------------------------------------------
%%% Функциии тестирования
%%% -------------------------------------------------------------------------

is_proplist([]) ->
    true;
is_proplist([{K,_}|R]) when is_atom(K) ->
    %%% {key, _}
    is_proplist(R);
is_proplist([{K,_}|R]) when is_list(K) ->
    %%% {"key", _}
    is_proplist(R);

is_proplist(_) ->
    false.

test()->

    ok.


test(speed)->
    ok.
