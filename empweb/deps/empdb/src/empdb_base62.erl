%% abase62 encoding - initial idea based on riak's encoding, with no if's and fixed base
%% gleicon - 2010
%%
%% testing:
%% 0> c(abase62.erl).
%% 1> abase62:decode(abase62:encode(1000)) == 1000.
%%

-module(empdb_base62).
-export([encode/1, decode/1]).

nthchar(N) when N =< 9 -> $0 + N;
nthchar(N) when N =< 35 -> $A +N - 10;
nthchar(N) -> $a + N - 36.


encode(Id) -> encode(Id, []).
encode(Id, Acc) when Id < 0 -> encode(-Id, Acc);
encode(Id, []) when Id =:= 0 -> "0";
encode(Id, Acc) when Id =:= 0 -> Acc;
encode(Id, Acc) ->
    R = Id rem 62,
    Id1 = Id div 62,
    Ac1 = [nthchar(R)|Acc],
    encode(Id1, Ac1).

decode(S) -> decode(S, 0).
decode([C|Cs], Acc)  when C >= $0, C =< $9 -> decode(Cs, 62 * Acc + (C - $0));
decode([C|Cs], Acc)  when C >= $A, C =< $Z -> decode(Cs, 62 * Acc + (C - $A + 10));
decode([C|Cs], Acc)  when C >= $a, C =< $z -> decode(Cs, 62 * Acc + (C - $a + 36));
decode([], Acc) -> Acc.
