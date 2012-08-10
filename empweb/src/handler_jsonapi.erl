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
    ?evman_args([Req, State]),

    Empweb_resp  =
        case cowboy_http_req:method(Req) of
            {'POST', _} ->
                handle_post(Req, State);
            _ ->
                jsonapi:method_not_allowed()
        end,

    ?evman_debug({empweb_resp, Empweb_resp}, <<"empweb response">>),

    Http_resp = empweb_http:resp(Empweb_resp),

    ?evman_debug({http_resp, Http_resp}, <<"http response">>),

    Http_resp_json = ejson:encode(Http_resp#http_resp.body),

    ?evman_debug({http_resp_json, Http_resp_json}, <<"http json">>),

    {ok, Reply} = cowboy_http_req:reply(
        Http_resp#http_resp.status,
        Http_resp#http_resp.headers,    
        Http_resp_json,
        Req
    ),

    ?evman_debug({reply, Reply}, <<"server reply">>),

    {ok,Reply,State}.

handle_post(Req, State)->
    ?evman_args([Req, State]),

    case cowboy_http_req:body_qs(Req) of
            {Pbody, _} ->
                handle_post_body(Req, State, Pbody);
            _ ->
                jsonapi:not_extended(no_post_body)
        end.

handle_post_body(Req, State, Pbody)->
    ?evman_args([Req, State, Pbody]),

    case proplists:get_value(<<"data">>, Pbody) of
        undefined ->
            jsonapi:not_extended(no_data);
        Bobject ->
            handle_data(Req, State, Bobject)
    end.

handle_data(Req, State, Bobject)->
    
    try
        ?evman_debug({bobject, Bobject}, <<"binary object">>),
        Object  =  ejson:decode(Bobject),
        ?evman_debug({object, Object},  <<"native object">>),
        jsonapi_map(Req, Object)
    catch
        throw:{invalid_json, _} ->
            jsonapi:not_extended(invalid_json);
        Eclass:Ereason ->
            ?evman_error(#event{error={Eclass,Ereason}}),
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
    ?evman_args([{List}]),

    Fname   =  proplists:get_value(<<"fname">>, List),
    Params  =  case proplists:get_value(<<"params">>, List, []) of
        null ->
            [];
        Res ->
            Res
    end,

    ?debug("Params   = ~p~n",[Params]),

    Is_auth=biz_session:is_auth(empweb_http:auth_cookie(Req)),

    ?evman_debug({jsonapi_call, [
        {fname,     Fname},
        {params,    Params},
        {is_auth,   Is_auth}
    ]}),

    Action =
        case Fname of
            <<"register">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action='register',
                    params=Params
                };

            %%
            %% {"fname": "login", "params":{"nick":"admin", "pass":"admin"}} 
            %%
            <<"login">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=login,
                    params=Params
                };

            %%
            %% {"fname": "logout", "params":{"nick":"admin"}}
            %%
            <<"logout">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=logout,
                    params=Params
                };

            %%
            %% {"fname": "update_user", "params":{"id":1,"hobby":"some a"}} 
            %%
            <<"update_user">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=update_user,
                    params=Params,
                    is_auth=Is_auth
                };

            %%
            %% {"fname": "get_user", "params":{"nick":"admin"}}
            %%
            <<"get_user">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=get_user,
                    params=Params,
                    is_auth=Is_auth
                };

            %%
            %% {"fname": "get_all_users", "params":{}} 
            %%
            <<"get_all_users">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=get_all_users,
                    params=Params,
                    is_auth=Is_auth
                };

            %%
            %% {"fname": "get_friends", "params":{"user_id":1}} 
            %%
            <<"get_friends">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=get_friends,
                    params=Params,
                    is_auth=Is_auth
                };

            %%
            %%  {
            %%      "fname": "add_friend",
            %%      "params":{"user_id":1, "friend_id":1}
            %%  }
            %%
            <<"add_friend">> ->
                #empweb_hap{
                    handler=jsonapi_user,
                    action=add_friend,
                    params=Params,
                    is_auth=Is_auth
                };

            %%
            %%  {
            %%      "fname": "delete_friend",
            %%      "params":{"user_id":1, "friend_id":1}
            %%  } 
            %%
            <<"delete_friend">> ->  
                #empweb_hap{
                    handler=jsonapi_user,
                    action=delete_friend,
                    params=Params,
                    is_auth=Is_auth
                };

            _ -> []
        end,

    ?evman_debug({jsonapi_action, Action}),
    
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

jsonapi_map(_req, List) ->
    ?evman_args([List]),
    
    jsonapi:not_extended(wrong_format).

