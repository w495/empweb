-module(empweb_handler_jsonapi_test).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-define(AUTH_COOKIE_NAME, <<"empire_100829481802076318">>).


init(_, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    io:format("handle ~p ~n", [?LINE]),
    {Code, Repdata, Reqres} = case cowboy_http_req:method(Req) of
            {'POST', Req1} ->
                handle_post(Req1, State);
            {_, Req1} ->
                {405, <<"{\"error\": \"method_not_allowed\"}">>, Req1}
        end,
    io:format("handle ~p ~n", [?LINE]),
    Reply = cowboy_http_req:reply(Code,
        [{<<"Content-Type">>,  <<"application/json;charset=UTF-8">>}],
        Repdata,
        Reqres
    ),
    io:format("handle ~p ~n", [?LINE]),
    {ok, Reply, State}.


handle_post(Req, State)->
    io:format("handle_post ~p ~n", [?LINE]),
    X = cowboy_http_req:body_qs(Req),
    io:format("~n cowboy_http_req:body_qs(Req) = ~p~n~n", [X]),
    io:format("handle_post ~p ~n", [?LINE]),
    {Pbody, Req1} = X,
    io:format("handle_post ~p ~n", [?LINE]),
    handle_post_body(Req1, State, Pbody).

handle_post_body(Req, State, Pbody)->
    io:format("handle_post_body ~p ~n", [?LINE]),
    case proplists:get_value(<<"data">>, Pbody) of
        undefined ->
            {510, <<"{\"error\": \"no_data\"}">>, Req};
        Data ->
            handle_data(Req, State, Data)
    end.

handle_data(Req, State, Data)->
    io:format("handle_data ~p ~n", [?LINE]),
    {_aobj, Req1} = auth(Req),
    {200, Data, Req1}.

terminate(_Req, _State) ->
    ok.

auth(Req)->
    auth_cookie(Req).

auth_cookie(Req)->
    cowboy_http_req:cookie(?AUTH_COOKIE_NAME, Req).
