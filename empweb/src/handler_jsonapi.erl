%% Feel free to use, reuse and abuse the code in this file.

-module(handler_jsonapi).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).


-include("empweb.hrl").

-include_lib("norm/include/norm.hrl").

%%
%% Описание записей событий и макросов
%%
-include_lib("evman/include/events.hrl").


%%
%% Трансформация для получения имени функции. 
%%
-include_lib("evman/include/evman_transform.hrl").


%%
%% =========================================================================
%% =========================================================================
%%


init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    ?evman_funcall([Req, State]),

    Empweb_resp  =
        case cowboy_http_req:method(Req) of
            {'POST', _} ->
                handle_post(Req, State);
            _ ->
                jsonapi:method_not_allowed()
        end,

    ?debug("0~n"),
    ?debug("Empweb_resp ~p ~n", [Empweb_resp]),

    Http_resp = empweb_http:resp(Empweb_resp),

    ?debug("1~n"),

    X = ejson:encode(Http_resp#http_resp.body),

    ?debug("2~n"),

    {ok, Reply} = cowboy_http_req:reply(
        Http_resp#http_resp.status,
        Http_resp#http_resp.headers,    
        X,
        Req
    ),
    
    ?debug("n~n"),

    {ok,Reply,State}.

handle_post(Req, State)->
    ?evman_funcall([Req, State]),

    case cowboy_http_req:body_qs(Req) of
            {Post_body, _} ->
                handle_post_body(Req, State, Post_body);
            _ ->
                jsonapi:not_extended(no_post_body)
        end.

handle_post_body(Req, State, Post_body)->
    ?evman_funcall([Req, State, Post_body]),

    case proplists:get_value(<<"data">>, Post_body) of
        undefined ->
            jsonapi:not_extended(no_data);
        Binary_object ->
            handle_data(Req, State, Binary_object)
    end.

handle_data(Req, State, Binary_object)->
    
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
            %% {"fname": "login", "params": {"id": 1}}
            
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

