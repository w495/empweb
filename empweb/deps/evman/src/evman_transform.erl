-module(evman_transform).

-export([parse_transform/2]).

-include("evman.hrl").

parse_transform(AST, _Options) ->
    [parse(T) || T <- AST].

parse({function, _, FName, FArity, _} = T) ->
    erl_syntax_lib:map(
        fun(TE) ->
            parsemacro(FName, FArity, TE)
        end,
        T
    );

parse(T) ->
    T.

parsemacro(FName, FArity, T) ->
    erl_syntax:revert(
        case erl_syntax:type(T) of
            atom ->
                case erl_syntax:atom_value(T) of
                    ?FUNCTION -> erl_syntax:atom(FName);
                    ?ARITY -> erl_syntax:integer(FArity);
                    _ -> T
                end;
            _ ->
                T
        end
    ).  
