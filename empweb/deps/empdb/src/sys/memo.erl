%%% 
%%% @file memo.erl  Простая мемоизация.
%%%
%%%     Функциям '[l|p]memoize'
%%%         на вход подается
%%%             функция от 0\1 аргумента;
%%%         результатом тоже является функция от 0\1 аргументов.
%%%     Функциям '[l|p]save'
%%%         на вход подается
%%%             функция от N аргументов,
%%%             список аргуменитов;
%%%         a результатом является результат вычисления
%%%             функции на этих аргументах.
%%%     Функции 'rmemoize'
%%%         на вход подается
%%%             функция, с аргуметом функцией одного аргумента,
%%%             и возвращающая функцию одного аргумента;
%%%         a результатом тоже является функция от 1 аргументa.
%%%     Примеры использования даны в конце файла.
%%%
%%%     Мы умышленно не используем apply/3 и Mod:Name(Args).
%%%     Это позволяет нам не увеличивать время на вызов функции в 2 раза.
%%%         http://www.erlang.org/doc/efficiency_guide/functions.html#id67124
%%%

-module (memo).

-export ([
    pmemoize/1,
    lmemoize/1,
    psave/2,
    lsave/2,
    lsave_free/2
]).

-export ([
    rmemoize/1,
    test/0
]).

-export ([
    ploop/2
]).


%%% ------------------------------------------------------------------------
%%% Классическая рекурсивная мемоизация
%%% Реализована через оператор неподвижной точки.
%%% Крайне эффективна для рекурсивных функций.
%%% ------------------------------------------------------------------------

%%% 
%%% @doc
%%%     Оператор неподвижной точки
%%%     Следует из определения рекурсии в λ-исчислении.
%%%
ry(Function) when erlang:is_function(Function, 1) ->
    Function(
        fun(X) ->
            (ry(Function))(X)
        end
    ).

%%%
%%% @doc
%%%     Вычисляет рекурсивную функцию.
%%%     Запоминает промежуточные результаты.
%%%     И при необходимости достает их из памяти.
%%%
rmemoize(Tab, Function) when erlang:is_function(Function, 1) ->
    fun (B) ->
        Bfunction = Function(B),
        fun (Args) ->
            Hash = funhash(Bfunction, Args),
            case get_value(Tab, Hash) of
                undefined ->
                    Result = Bfunction(Args),
                    put_value(Tab, Hash, Result),
                    Result;
                Result ->
                    Result
            end
        end
    end.

%%%
%%% @doc
%%%     Вычисляет рекурсивную функцию.
%%%     Удаляет промежуточные результаты.
%%%     Запоминает последний результат.
%%%
%%%     ВАЖНО:
%%%         Function = fun( fun(Arg) ) -> fun(Arg)
%%%     Или в стиле Ocaml:
%%%         (fun (fun -> Arg ) -> (fun -> Arg)
%%%
rmemoize(Function) when erlang:is_function(Function, 1) ->
    fun (Args) ->
        Anstab = new(memo_rmemoize_ans),
        Hash = funhash(Function, Args),
        case get_value(Anstab, Hash) of
            undefined ->
                Tmptab = new(memo_rmemoize_tmp),
                Mfunction = rmemoize(Tmptab, Function),
                Yfunction = ry(Mfunction),
                Ans = Yfunction(Args),
                delete(Tmptab),
                put_value(Anstab, Hash, Ans),
                Ans;
            Result ->
                Result
        end
    end.


%%% ------------------------------------------------------------------------
%%%     Линейная мемоизация, по сути является
%%%         обычным хешированием без определенного времени удаления
%%%
%%%     Сохраняется только конечный результат вычислений.
%%%     Промежуточные варианты, для рекурсивной функции не сохраняются.
%%%         но при соответсвуюущей мемоизация рекурсивнх вызовов
%%%             --- это возможно.
%%%
%%% ------------------------------------------------------------------------

%%%
%%% @doc
%%%     Вычисляет функцию.
%%%     Запоминает последний результат.
%%%
%%%     ВАЖНО:
%%%         Function = fun() 
%%%         Function = fun(Arg)
%%%
lmemoize(Function) when erlang:is_function(Function, 0) ->
    fun (Args) ->
        lmemoize_inside(Function, 0, Args)
    end;
lmemoize(Function) when erlang:is_function(Function, 1) ->
    fun (Args) ->
        lmemoize_inside(Function, 1, Args)
    end.

lmemoize_inside(Function, Arity, Args) ->
    Anstab = new(memo_lmemoize_ans),
    Hash = funhash(Function, Args),
    case get_value(Anstab, Hash) of
        undefined ->
            Ans = apply_one(Function, Arity, Args),
            put_value(Anstab, Hash, Ans),
            Ans;
        Result ->
            Result
    end.

%%%
%%% @doc
%%%     Вычисляет функцию.
%%%     Запоминает последний результат.
%%%     Для каждого вызова функции создается
%%%         отдельный процесс.
%%%     Последнее может быть полезно,
%%%         если в качестве хранилища будем использовать
%%%             локальную память процесса в виде состояния
%%%                 (dict, gb_tree, proplist).
%%%     ВАЖНО:
%%%         Function = fun()
%%%         Function = fun(Arg)
%%%
pmemoize(Function) when erlang:is_function(Function, 0) ->
    fun() ->
        pcall(Function, 0, undefined)
    end;

pmemoize(Function) when erlang:is_function(Function, 1) ->
    fun(Args) ->
        pcall(Function, 1, Args)
    end.

pspawn(Function) ->
    spawn(?MODULE, ploop, [new(memo_pmemoize_ans), Function]).

pcall(Function, Arity, Args) ->
    pspawn(Function) ! {memo, self(), Arity, Args},
    receive
        {_, {'EXIT', Reason}} -> erlang:error(Reason);
        {fresh, Value}        -> Value;
        {cache, Value}        -> Value
    end.

ploop(Tab, Fun) ->
    receive
        {memo, From, Arity, Args} ->
            Hash = funhash(Fun, Args),
            case get_value(Tab, Hash) of
                undefined   ->
                    Result = apply_one(Fun, Arity, Args),
                    From ! {fresh, Result},
                    put_value(Tab, Hash, Result),
                    ploop(Tab, Fun);
                Result ->
                    From ! {cache, Result},
                    ploop(Tab, Fun)
            end
    end.

%%% ------------------------------------------------------------------------
%%% 
%%% ------------------------------------------------------------------------


lsave(Function, Argslist)
        when erlang:is_function(Function)
                andalso erlang:is_list(Argslist) ->
    Anstab = new(memo_lsave_ans),
    Hash = funhash(Function, Argslist),
    case get_value(Anstab, Hash) of
        undefined ->
            Ans =  erlang:apply(Function, Argslist),
            put_value(Anstab, Hash, Ans),
            Ans;
        Result ->
            Result
    end.

lsave_free(Function, Argslist)
        when erlang:is_function(Function)
                andalso erlang:is_list(Argslist) ->
    Hash = funhash(Function, Argslist),
    delete(memo_lsave_ans, Hash).


psave(Function, Argslist)
        when erlang:is_function(Function)
                andalso erlang:is_list(Argslist) ->
    erlang:error("Not implemementet").



%%% ------------------------------------------------------------------------
%%% Вспомогательные функции
%%% ------------------------------------------------------------------------

apply_one(Fun, Arity, Args)->
    case Arity of
        0 ->
            Fun();
        _ ->
            Fun(Args)
    end.
%%% 
%%% @doc 
%%%     Вычисляет хеш для функции и аргументов.
%%% 
funhash(Fun, Args) ->
    {module, Module}    = erlang:fun_info(Fun, module),
    {name, Name}        = erlang:fun_info(Fun, name),
    {arity, Arity}      = erlang:fun_info(Fun, arity),
    {Module, Name, Arity, Args}.
    %erlang:md5(erlang:term_to_binary({Module, Name, Arity, Args})).


new() ->
    new(?MODULE).

new(Tab) ->
    storage:newc(Tab).

delete() ->
    delete(?MODULE).

delete(Tab) ->
    storage:delete(Tab).

delete(Tab, Key) ->
    storage:delete(Tab, Key).

get_value(Args) ->
    get_value(?MODULE, Args).

get_value(Tab, Args) ->
    storage:value(Tab, Args).

put_value(Args, Result) ->
    put_value(?MODULE, Args, Result).

put_value(Tab, Args, Result) ->
    storage:insert(Tab, Args, Result).

%%% ------------------------------------------------------------------------
%%% Функции тектирования
%%% ------------------------------------------------------------------------

test_fibimpl_rmemoize(Self) ->
    fun
        (0) -> 1;
        (1) -> 1;
        (N) when N > 1 ->
            ((Self)(N - 1)) + ((Self)(N - 2))
    end.

test_fff_rmemoize(Self) ->
    fun(N)->
         N
    end.

test_fib_rmemoize(N) ->
    Function1 = rmemoize(fun test_fibimpl_rmemoize/1),
    io:format("test_fibimpl_rmemoize(~p) = ~p ~n",
        [N, Function1(N)]),
    ok.


%%% ------------------------------------------------------------------------

test_simple_fibimpl_pmemoize(0) -> 1;
test_simple_fibimpl_pmemoize(1) -> 1;
test_simple_fibimpl_pmemoize(N) when N > 1 ->
    test_simple_fibimpl_pmemoize(N-1) + test_simple_fibimpl_pmemoize(N - 2).

test_simple_fib_pmemoize(N) ->
    Function1 = pmemoize(fun test_simple_fibimpl_pmemoize/1),
    io:format("test_simple_fibimpl_pmemoize(~p) = ~p ~n",
        [N, Function1(N)]),
    ok.

%%% ------------------------------------------------------------------------

test_rec_fibimpl_pmemoize(0) -> 1;
test_rec_fibimpl_pmemoize(1) -> 1;
test_rec_fibimpl_pmemoize(N) when N > 1 ->
    Function = pmemoize(fun test_rec_fibimpl_pmemoize/1),
    Function(N-1) + Function(N - 2).

test_rec_fib_pmemoize(N) ->
    io:format("test_rec_fibimpl_pmemoize(~p) = ~p ~n",
        [N, test_rec_fibimpl_pmemoize(N)]),
    ok.

%%% ------------------------------------------------------------------------

test_simple_fibimpl_lmemoize(0) -> 1;
test_simple_fibimpl_lmemoize(1) -> 1;
test_simple_fibimpl_lmemoize(N) when N > 1 ->
    test_simple_fibimpl_lmemoize(N-1) + test_simple_fibimpl_lmemoize(N - 2).

test_simple_fib_lmemoize(N) ->
    Function1 = lmemoize(fun test_simple_fibimpl_lmemoize/1),
    io:format("test_simple_fibimpl_lmemoize(~p) = ~p ~n",
        [N, Function1(N)]),
    ok.

%%% ------------------------------------------------------------------------

test_rec_fibimpl_lmemoize(0) -> 1;
test_rec_fibimpl_lmemoize(1) -> 1;
test_rec_fibimpl_lmemoize(N) when N > 1 ->
    Function = lmemoize(fun test_rec_fibimpl_lmemoize/1),
    Function(N-1) + Function(N - 2).

test_rec_fib_lmemoize(N) ->
    io:format("test_rec_fibimpl_lmemoize(~p) = ~p ~n",
        [N, test_rec_fibimpl_lmemoize(N)]),
    ok.

%%% ------------------------------------------------------------------------

test()->
    io:format("test~n"),
    test_fib_rmemoize(10000).



