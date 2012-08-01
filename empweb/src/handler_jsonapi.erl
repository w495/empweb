%% Feel free to use, reuse and abuse the code in this file.

-module(handler_jsonapi).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).




-include("empweb.hrl").

-include_lib("norm/include/norm.hrl").

%%
%% =========================================================================
%% =========================================================================
%%
-compile({parse_transform, zt}).

-define(FUNCTION, '__function_macro__').
-define(ARITY, '__function_arity__').
-define(ARGS, '__function_args__').

-record(event_fun, {
    module      =   ?MODULE,
    function    =   ?FUNCTION,
    arity       =   ?ARITY,
    line        =   ?LINE
}).

-record(event_data, {
    args    = [],
    data    = [],
    log     = [],
    debug   = [],
    error   = [],
    info    = []
}).


-record(event, {
   'fun' = #event_fun{},
    data = #event_data{},
    datetime = erlang:now()
}).


-define(event,
    #event{
        'fun'=#event_fun{function=?FUNCTION, arity=?ARITY, args}
    }
).

-define(event(Data),
    #event{
        'fun'   = #event_fun{function=?FUNCTION, arity=?ARITY},
         data   = Data
    }
).


-define(eventd, ?event).

-define(eventd(Data),
    ?event(#event_data{Data})
).


-define(notify, evman:note(?eventd)).

-define(notify(Data), evman:note(?eventd(Data))).


init({tcp, http}, Req, _Opts) ->
    ?debug("init({tcp, http}, Req, _Opts) ->~p~n", [{?MODULE, ?FUNCTION, ?ARITY}]),
    {ok, Req, undefined_state}.

handle(Req, State) ->
    ?notify(args=[Req, State]),
    
    Empweb_resp  =
        case cowboy_http_req:method(Req) of
            {'POST', _} ->
                ?debug("handle(Req, State) ->~n"),
                handle_post(Req, State);
            _ ->
                jsonapi:method_not_allowed()
        end,
    ?debug("-> 1 ~n"),
    Http_resp = empweb_http:resp(Empweb_resp),
    ?debug("-> Http_resp ~p ~n", [Http_resp]),
    X = ejson:encode(Http_resp#http_resp.body),
    ?debug("-> X ~p ~n", [X]),
    {ok, Reply} = cowboy_http_req:reply(
        Http_resp#http_resp.status,
        Http_resp#http_resp.headers,    
        X,
        Req
    ),
    ?debug("-> Reply ~p ~n", [Reply]),
    {ok,Reply,State}.

handle_post(Req, State)->
    ?notify(args=[Req, State]),

    ?debug("-> handle_post ~n"),
    case cowboy_http_req:body_qs(Req) of
            {Post_body, _} ->
                handle_post_body(Req, State, Post_body);
            _ ->
                jsonapi:not_extended(no_post_body)
        end.

handle_post_body(Req, State, Post_body)->
    case proplists:get_value(<<"data">>, Post_body) of
        undefined ->
            jsonapi:not_extended(no_data);
        Binary_object ->
            handle_data(Req, State, Binary_object)
    end.

handle_data(Req, _state, Binary_object)->
    try
        ?debug("Binary_object =  ~p~n", [Binary_object]),
        Object  =  ejson:decode(Binary_object),
        ?debug("Object  =  ~p~n", [Object]),
        jsonapi_map(Req, Object)
    catch
        throw:{invalid_json, _} ->
            jsonapi:not_extended(invalid_json);
        Eclass:Ereason ->
            ?debug("~p ~p ~n", [Eclass, Eclass]),
            jsonapi:internal_server_error(
                {[
                    {unknown_error, 
                        {[
                            {class, jsonapi:format(Eclass)},
                            {reason, jsonapi:format(Ereason)}
                        ]}
                    }
                ]}
            )
    end.

terminate(_Req, _State) ->
    ok.

jsonapi_map(Req, {List}) ->
    Fname  =  proplists:get_value(<<"fname">>, List),
    Params  =  proplists:get_value(<<"params">>, List),

    Is_auth=biz_session:is_auth(empweb_http:auth_cookie(Req)),

    Action =
        case Fname of
            <<"register">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action='register',
                    params=Params
                };
            <<"login">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=login,
                    params=Params
                };
            <<"logout">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=logout,
                    params=Params
                };
            <<"update_user">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=update_user,
                    params=Params,
                    is_auth=Is_auth
                };
            <<"get_friends">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=get_friends,
                    params=Params,
                    is_auth=Is_auth
                };
            <<"add_friend">> ->     
                #empweb_hap{
                    handler=jsonapi_user,
                    action=add_friend,
                    params=Params,
                    is_auth=Is_auth
                };
            <<"delete_friend">> ->  
                #empweb_hap{
                    handler=jsonapi_user,
                    action=delete_friend,
                    params=Params,
                    is_auth=Is_auth
                };
            <<"get_user">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=get_user,
                    params=Params,
                    is_auth=Is_auth
                };
            _ -> []
        end,

    ?debug("Action = (~p)", [Action]),
    
    case empweb_http:call(Req, Action) of
        {ok, Reply} ->
            Reply;
        {error, unknown_function} ->
            jsonapi:not_extended(unknown_function);
        {error, Error} ->
            jsonapi:internal_server_error(
                {[{unknown_error, jsonapi:format(Error)}]}
            )
    end;

jsonapi_map(_req, _x) ->
    jsonapi:not_extended(wrong_format).

