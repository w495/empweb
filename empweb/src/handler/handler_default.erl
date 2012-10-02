%% Feel free to use, reuse and abuse the code in this file.

-module(handler_default).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-include("empweb.hrl").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

%%
%% @doc Возвращает 404, вместе с путем, если его размер менее 64.
%%
handle(Req, State) ->
    {ok, Resreq} = 
        cowboy_http_req:reply(
            404, 
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

terminate(_Req, _State) ->
    ok.
