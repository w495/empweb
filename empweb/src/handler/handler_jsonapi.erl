%% Feel free to use, reuse and abuse the code in this file.

-module(handler_jsonapi).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-include("norm.hrl").
-include("empweb.hrl").

-include_dir("user.hrl"). %%% from empdb


init({tcp, http}, Req, _Opts) ->
    io:format("init({tcp, http}, Req, _Opts) ->"),
    {ok, Req, undefined_state}.

handle(Req, State) ->
    io:format("handle(Req, State) ->"),
    Reply  =  case cowboy_http_req:method(Req) of
        {'POST', _} ->
            Post_msg  =  case cowboy_http_req:body_qs(Req) of
                {X, _} -> case proplists:get_value(<<"data">>, X) of
                    undefined -> <<"no echo_post post parameter">>;
                    Y -> Y,
                    {ok, R}  =  try
                        Object  =  ejson:decode(Y),
                        io:format("  =  ~p~n", [Object]),
                        Result  =  jsonmap(Object),
                        cowboy_http_req:reply(200, ?OUTPUT_JSON_HEADERS, ejson:encode(Result), Req)
                    catch
                        throw:{invalid_json, _} ->
                            cowboy_http_req:reply(200, ?OUTPUT_JSON_HEADERS, ejson:encode({[{<<"error">>, <<"invalid json">>}]}), Req);
                        T:E ->
                        io:format("~p ~p ~n", [T, E]),
                        cowboy_http_req:reply(200, ?OUTPUT_JSON_HEADERS, ejson:encode({[{<<"error">>, <<"unknown error">>}]}), Req)
                    end,
                    R
                end
            end;
        _ ->
            {ok, R}  =  cowboy_http_req:reply(200, ?OUTPUT_JSON_HEADERS, ejson:encode({[{<<"error">>, <<"not post">>}]}), Req),
            R
    end,
    {ok, Reply, State}.

terminate(_Req, _State) ->
    ok.

jsonmap({List}) ->
    Fname  =  proplists:get_value(<<"fname">>, List),
    Params  =  proplists:get_value(<<"params">>, List),
    jsonmap(Fname, Params);

jsonmap(X) ->
    {[{<<"error">>, <<"wrong format">>}]}.

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
jsonmap(<<"register">>, Params) ->
    Data  =  norm:norm(Params, [
        #norm_rule{
            key = nick,
            types = [string]
        },
        #norm_rule{
            key = description, 
            types = [string]
        },
        #norm_rule{
            key = pass,
            types = [string]
        },
        #norm_rule{
            key = email,
            types = [email]
        },
        #norm_rule{
            key = phone,
            types = [string]
        },
        #norm_rule{
            key = fname,
            types = [string]
        },
        #norm_rule{
            key = sname,
            types = [string]
        },
        #norm_rule{
            key = birthday,
            types = [string]
        },
        #norm_rule{
            key = city,
            types = [string]
        }
    ]),
    
    handle_params(Data, fun(Data)->
        biz_user:register(Data#norm.return)
    end);

%%%
%%% {"fname":"login", "params": {"nick":"admin", pass:"pass"}}
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
jsonmap(<<"get_friends">>, Params) ->
    io:format("Params = ~p", [Params]),
    Data  =  norm:norm(Params, [
        #norm_rule{
            key = user_id,
            types = [integer]
        }
    ]),
    handle_params(Data, fun(Data)->
        biz_user:get_friends(Data#norm.return)
    end);

%%%
%%% {"fname":"add_friend", "params": {"user_id":12, friend_id:1}}
%%%
jsonmap(<<"add_friend">>, Params) ->
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
        biz_user:add_friend(Data#norm.return)
    end);


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

