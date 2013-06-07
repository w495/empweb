%% @copyright 2013 Empire
%% @author Илья w-495 Никитин
%% @doc Генерация UUID по алгоритму V4.
%%

-module(empdb_uuid).

-export([
    v4/0,
    to_string/1,
    get_parts/1
]).

%% ==========================================================================
%% Спецификации
%% ==========================================================================

-spec   v4()                                            -> binary().
-spec   to_string(binary())                             -> binary().
-spec   get_parts(binary())                             -> list().
-spec   v4(integer(), integer(), integer(), integer())  -> binary().

%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% @spec    v4() -> binary().
%%
v4() ->
    v4(
        random:uniform(round(math:pow(2, 48))) - 1,
        random:uniform(round(math:pow(2, 12))) - 1,
        random:uniform(round(math:pow(2, 32))) - 1,
        random:uniform(round(math:pow(2, 30))) - 1
    ).

%%
%% @spec    to_string(binary()) -> binary().
%%
to_string(Binary) ->
    erlang:list_to_binary(
        io_lib:format(
            "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
            get_parts(Binary)
        )
    ).

%%
%% @spec    get_parts(binary()) -> list().
%%
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].

%% ==========================================================================
%% Внутренние функции
%% ==========================================================================

%% @spec   v4(integer(),integer(),integer(),integer()) -> binary().
v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.
