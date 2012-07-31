-module(jsonapi).

-include("empweb.hrl").

-include_lib("norm/include/norm.hrl").


-export([
    handle_params/2,
    is_auth/3,
    is_auth/2
]).

handle_params(Data, Function) ->
    io:format("handle_params(Data, Function) -> ~p ~p ~n", [Data, Function]),

    case Data#norm.errors of
        [] ->
            case erlang:apply(Function, [Data]) of
                {ok, Reply, State} ->
                    io:format("Reply -> ~p ~n", [Reply]),
                    {ok, Reply, State};
                {error, Error} ->
                    X=erlang:list_to_binary(io_lib:format("~p",[Error])),
                    io:format("X = ~p", [Error]),
                    {error, Error};
                _ ->
                    io:format("handle_params(Data, Function) -> ~p ~p ~n", [Data, Function])
            end;
        Error ->
            #empweb_resp{
                status  = 200,
                format = json,
                body = {[{<<"error">>, <<"wrong format">>}]}
            }
    end.



is_auth(Req, Function, Args)->
    io:format("is_auth(Req, Function, Args)->~n"),
    case biz_session:get(empweb_http:auth_cookie(Req)) of
        [] ->
            {ok, #empweb_resp{
                status  = forbidden,
                format = json,
                body = {[{<<"error">>, <<"forbidden">>}]}
            }, []};
        [_H|_T] ->
            X = erlang:apply(Function, Args),
            io:format("X  -> ~p ~n", [X]),
            X 
    end.

is_auth(Req, Function)->
    is_auth(Req, Function, []).
