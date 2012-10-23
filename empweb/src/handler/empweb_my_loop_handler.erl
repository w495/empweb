-module(empweb_my_loop_handler).
-export([
    init/3,
    info/3,
    handle/2,
    terminate/2
]).

-include("empweb.hrl").

-define(TIMEOUT, 60000).


init({tcp, http}, Req, Opts) ->
    ?debug("--> init ~p <-- ~n", [self()]),
    {loop, Req, start, ?TIMEOUT}.

handle(Req, State) ->
    ?debug("--> ~p <-- ~n", [?LINE]),
    {ok, Resreq} =
        cowboy_http_req:reply(
            200,
            ?OUTPUT_TEXT_HEADERS,
            case
                begin
                    {Path, _} = cowboy_http_req:raw_path(Req),
                    {Path, erlang:size(Path) > 64 }
                end
            of
                {_, true} ->
                    <<"Not found">>;
                {Path, _} ->
                    [<<"Not found: ">>, Path]
            end,
            Req
        ),
    {ok, Resreq, State}.

info({reply, Body}, Req, start) ->
    ?debug("--> ~p <-- ~n", [?LINE]),
    {ok, Resreq} = cowboy_http_req:chunked_reply(200, Req),
    {loop, Resreq, work};

info({reply, Body}, Req, work) ->
    ?debug("--> ~p <-- ~n", [?LINE]),
    ok = cowboy_http_req:chunk(Body, Req),
    {loop, Req, work};

info(die, Req, work) ->
    ?debug("--> ~p <-- ~n", [?LINE]),
    ok = cowboy_http_req:chunk("stop\r\n", Req),
    {loop, Req, stop};

info(Body, Req, stop) ->
    %?debug("--> ~p <-- ~n", [?LINE]),
    %{ok, Resreq} = cowboy_http_req:reply(200, [], Body, Req),
    %{ok, Resreq, State}.
    {loop, Req, stop}.

terminate(Req, State) ->
    ?debug("--> terminate ~p <-- ~n", [?LINE]),
    ok.