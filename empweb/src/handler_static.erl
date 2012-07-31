%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @doc Static File Cowboy Handler.

-module(handler_static).

-behaviour(cowboy_http_handler).

%% cowboy_http_handler api
-export([init/3, handle/2, terminate/2]).

-record(state, {path=undefined, method=undefined, exists=undefined,
        filepath=undefined}).

%% ----------------------------------------------------------------------------
%% cowboy_http_handler api
%% ----------------------------------------------------------------------------

init({_Transport, http}, Req, Opts) ->
    io:format("<init>~n"),
    io:format("Req = ~p ~n", [Req]),
    erlang:append(erlang:tuple_to_list(Req), [some]),
    io:format("</init>~n"),
    case lists:keyfind(path, 1, Opts) of
        {path, Path} ->
            {ok, Req, #state{path=Path}};
        false ->
            {error, "No Path Given"}
    end.

handle(Req, State) ->
    {Method, Req2} = cowboy_http_req:method(Req),
    io:format("<handle>~n"),
    io:format("Req = ~p ~n", [Req]),
    io:format("</handle>~n"),
    method_allowed(Req2, State#state{method=Method}).

terminate(Req, _State) ->
    io:format("<terminate>~n"),
    io:format("Req = ~p ~n", [Req]),
    io:format("</terminate>~n"),
    ok.


%% ----------------------------------------------------------------------------
%% internal api
%% ----------------------------------------------------------------------------

%% @doc Join binary tokens in lists together using the unix path delimiter /
-spec filename_join(list(binary())) -> binary().
filename_join(Tokens) ->
    lists:foldl(
        fun
            (Token, <<>>) when is_binary(Token) ->
                <<Token/binary>>;
            (Token, <<>>) when is_list(Token) ->
                Rest = filename_join(Token),
                <<Rest/binary>>;
            (Token, Path) when is_list(Token) ->
                Rest = filename_join(Token),
                <<Path/binary, "/", Rest/binary>>;
            (Token, Path) when is_binary(Token) ->
                <<Path/binary, "/", Token/binary>>
        end, <<>>, Tokens).

method_allowed(Req, State=#state{method='GET'}) ->
    resource_exists(Req, State);
method_allowed(Req, State=#state{method='HEAD'}) ->
    resource_exists(Req, State);
method_allowed(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(405, [], <<>>, Req),
    {ok, Req2, State}.

resource_exists(Req, State) ->
    {PathTokens, Req2} = cowboy_http_req:path_info(Req),
    FilePath = filename_join([State#state.path | PathTokens]),
    case filelib:is_regular(FilePath) of
        true ->
            valid_request(Req2, State#state{exists=true, filepath=FilePath});
        false ->
            {ok, Req3} = cowboy_http_req:reply(404, [], <<>>, Req),
            {ok, Req3, State#state{exists=false, filepath=FilePath}}
    end.

valid_request(Req, State=#state{method='GET'}) ->
    {ok, Body} = file:read_file(State#state.filepath),
    {ok, Req2} = cowboy_http_req:reply(200, [], Body, Req),
    {ok, Req2, State};
valid_request(Req, State=#state{method='HEAD'}) ->
    FileSize = filelib:file_size(State#state.filepath),
    {ok, Req2} = cowboy_http_req:reply(200, [{<<"Content-Length">>, FileSize}], <<>>, Req),
    {ok, Req2, State}.
