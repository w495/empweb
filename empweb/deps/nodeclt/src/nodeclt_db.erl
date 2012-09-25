-module(nodeclt_db).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").


find(Q) ->
	F = fun() ->
			qlc:e(Q)
	end,
	transaction(F).

transaction(F) ->
	case mnesia:transaction(F) of
		{atomic, Result} ->
			Result;
		{aborted, _Reason} ->
			[]
	end.

new_id(Key) ->
	mnesia:dirty_update_counter({counter, Key}, 1).

read(Oid) ->
	F = fun() ->
			mnesia:read(Oid)
	end,
	transaction(F).
    %naming(Result, Names).

read_all(Table) ->
	Q = qlc:q([X || X <- mnesia:table(Table)]),
	find(Q).

write(Rec) ->
	F = fun() ->
			mnesia:write(Rec)
	end,
	mnesia:transaction(F).

delete(Oid) ->
	F = fun() ->
			mnesia:delete(Oid)
	end,
	mnesia:transaction(F).


