%% Feel free to use, reuse and abuse the code in this file.

-module(handler_jsonapi).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).


-include("empweb.hrl").

-include_lib("norm/include/norm.hrl").


init({tcp, http}, Req, _Opts) ->
    io:format("init({tcp, http}, Req, _Opts) ->~n"),
    {ok, Req, undefined_state}.

handle(Req, State) ->
    io:format("handle(Req, State) ->~n"),
    Empweb_resp  =
        case cowboy_http_req:method(Req) of
            {'POST', _} ->
                io:format("handle(Req, State) ->~n"),
                handle_post(Req, State);
            _ ->
                #empweb_resp{
                    status  = method_not_allowed,
                    format = json,
                    body = {[
                        {<<"error">>, <<"not post">>}
                    ]}
                }
        end,
    io:format("-> 1 ~n"),
    Http_resp = empweb_http:resp(Empweb_resp),
    io:format("-> Http_resp ~p ~n", [Http_resp]),
    X = ejson:encode(Http_resp#http_resp.body),
    io:format("-> X ~p ~n", [X]),
    {ok, Reply} = cowboy_http_req:reply(
        Http_resp#http_resp.status,
        Http_resp#http_resp.headers,    
        X,
        Req
    ),
    io:format("-> Reply ~p ~n", [Reply]),
    {ok,Reply,State}.

handle_post(Req, State)->
    io:format("-> handle_post ~n"),
    case cowboy_http_req:body_qs(Req) of
            {Post_body, _} ->
                handle_post_body(Req, State, Post_body);
            _ ->
                #empweb_resp{
                    status  = not_extended,
                    format = json,
                    body = {[
                        {<<"error">>, <<"no post body">>}
                    ]}
                }
        end.

handle_post_body(Req, State, Post_body)->
    case proplists:get_value(<<"data">>, Post_body) of
        undefined ->
            #empweb_resp{
                status  = not_extended,
                format = json,
                body = {[
                    {[{<<"error">>, <<"no data">>}]}
                ]}
            };
        Binary_object ->
            handle_data(Req, State, Binary_object)
    end.

handle_data(Req, State, Binary_object)->
    try
        io:format("Binary_object =  ~p~n", [Binary_object]),
        Object  =  ejson:decode(Binary_object),
        io:format("Object  =  ~p~n", [Object]),
        jsonapi_map(Req, Object)
    catch
        throw:{invalid_json, _} ->
            #empweb_resp{
                status  = not_extended,
                format = json,
                body = {[
                    {<<"error">>, <<"invalid json">>}
                ]}
            };
        T:E ->
            io:format("~p ~p ~n", [T, E]),
            #empweb_resp{
                status  = 500,
                format = json,
                body = {[
                    {<<"error">>, <<"unknown error">>}
                ]}
            }
    end.


terminate(_Req, _State) ->
    ok.

jsonapi_map(Req, {List}) ->
    Fname  =  proplists:get_value(<<"fname">>, List),
    Params  =  proplists:get_value(<<"params">>, List),

    Action =
        case Fname of
            <<"register">> ->       {jsonapi_user,'register',Params};
            <<"login">> ->          {jsonapi_user, login,    Params};
            <<"logout">> ->         {jsonapi_user, logout,   Params};
            <<"update_user">> ->    {jsonapi_user, update,   Params};
            
            <<"get_friends">> ->    {jsonapi_user, get_friends,   Params};
            <<"add_friend">> ->     {jsonapi_user, add_friend,   Params};
            <<"delete_friend">> ->  {jsonapi_user, delete_friend,   Params};
            
            <<"get_user">> ->       {jsonapi_user, 'get',    Params};
            
            _ -> ok
        end,

    io:format("Action = (~p)", [Action]),
    
    case empweb_http:call(Req, Action) of
        {ok, Reply} ->
            Reply;
        {error, Error} ->
            #empweb_resp{
                    status = 500,
                    format = json,
                    body   = {[
                        {<<"error">>,
                            erlang:list_to_binary(io_lib:format("~p",[Error]))
                        }
                    ]}
            }
    end;

jsonapi_map(Req, X) ->
    #empweb_resp{
        status  = not_extended,
        format = json,
        body = {[
            {<<"error">>, <<"wrong format">>}
        ]}
    }.





convert(Result) ->
    Result.


handle_params(Data, Function) ->
    case Data#norm.errors of
        [] ->
            io:format("Data#norm.return  =  ~p~n", [Data#norm.return]),
            case erlang:apply(Function, [Data]) of
                {ok, Result} -> 
                    {[{<<"ok">>, convert(Result)}]};
                ok -> 
                    {[{<<"ok">>, <<"ok">>}]};
                {error, Err} ->
                    X=erlang:list_to_binary(io_lib:format("~p",[Err])),
                    io:format("X = ~p", [X]),
                    {[{<<"error">>, X}]}
            end;
        Error ->
            {[{<<"error">>, <<"wrong format">>}]}
    end.
%%%
%% {
%%  "fname":"register", 
%%  "params": 
%%     {
%%      "nick":"some", 
%%      "pass":"pass",
%%      "description":"description",
%%      "email":"w@w-495.ru",
%%      "phone":"w@w-495.ru",
%%      "fname":"fname",
%%      "sname":"sname",
%%      "birthday" : "sdsd",
%%      "city":"city"
%%     }
%% }
%%%


%%%
%%% {"fname":"login", "params": {"nick":"admin", "pass":"pass"}}
%%%
jsonmap(<<"login">>, Params) ->
    io:format("Params = ~p", [Params]),
    Data  =  norm:norm(Params, [
        #norm_rule{
            key = nick,
            types = [string]
        },
        #norm_rule{
            key = pass,
            types = [string]
        }
    ]),
    io:format("Data = ~p", [Data]),
    io:format("Data#norm.return = ~p", [Data#norm.return]),
    handle_params(Data, fun(Data)->
        io:format("Data#norm.return = ~p", [Data#norm.return]),
        X =biz_user:login(Data#norm.return),
        io:format("X = ~p", [X]),
        X
    end);

%%%
%%% {"fname":"logout", "params": {"nick":"admin"}}
%%%
jsonmap(<<"logout">>, Params) ->
    io:format("Params = ~p", [Params]),
    Data  =  norm:norm(Params, [
        #norm_rule{
            key = nick,
            types = [string]
        }
    ]),
    io:format("Data = ~p", [Data]),
    io:format("Data#norm.return = ~p", [Data#norm.return]),
    handle_params(Data, fun(Data)->
        biz_user:logout(Data#norm.return)
    end);

%%%
%%% {"fname":"get_friends", "params": {"user_id":"12"}}
%%%

%%%
%%% {"fname":"add_friend", "params": {"user_id":12, friend_id:1}}
%%%


%%%
%%% {"fname":"delete_friend", "params": {"user_id":12, friend_id:1}}
%%%
jsonmap(<<"delete_friend">>, Params) ->
    io:format("Params = ~p", [Params]),
    Data  =  norm:norm(Params, [
        #norm_rule{
            key = user_id,
            types = [integer]
        },
        #norm_rule{
            key = friend_id,
            types = [integer]
        }
    ]),
    handle_params(Data, fun(Data)->
        biz_user:delete_friend(Data#norm.return)
    end);

jsonmap(<<"user_create">>, []) ->
    {[{<<"ok">>, 1}]};

jsonmap(<<"user_update">>, []) ->
    {[{<<"ok">>, 1}]};

jsonmap(<<"user_get">>, []) ->
    {[{<<"name">>, <<"fff">>}]};

jsonmap(<<"user_list">>, []) ->
    [{[{<<"name">>, <<"fff">>}]}];

jsonmap(Function, Params) ->
    {[{<<"error">>, <<"wrong format">>}]}.

