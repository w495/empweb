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


init(_, Req, _Opts) ->
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
        ?evman_debug({bobject, Bobject},        <<"binary object">>),
        Object  =  ejson:decode(Bobject),
        ?evman_debug({object, Object},          <<"native object">>),
        Res = jsonapi_map(Req, Object),
        ?evman_debug({jsonapi_result, Object},  <<"jsonapi result">>),
        Res 
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
    ?evman_debug({jsonapi_params, Params},  <<"jsonapi params">>),
    Aobj = empweb_http:auth(Req),
    
    ?evman_debug({jsonapi_aobj, Aobj},  <<"jsonapi aobj">>),

    Is_auth=            biz_pers:is_auth(Aobj),
    
    ?evman_debug({jsonapi_aobj, Aobj},  <<"jsonapi aobj">>),
    
    User_id=            biz_pers:get_pers_id(Aobj),
    User_perm_names=    biz_pers:get_perm_names(Aobj),

    

    ?evman_debug({jsonapi_call, [
        {fname,     Fname},
        {params,    Params},
        {is_auth,   Is_auth}
    ]}),

    Action =
        case Fname of
            <<"get_all_authorities">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_all_pstatuses',
                    params          =   Params
                };
            <<"get_authority">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_pstatus',
                    params          =   Params
                };

            <<"get_all_mstatuses">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_all_pstatuses',
                    params          =   Params
                };
            <<"get_mstatus">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_pstatus',
                    params          =   Params
                };

            <<"get_all_pstatuses">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_all_pstatuses',
                    params          =   Params
                };
            <<"get_pstatus">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_pstatus',
                    params          =   Params
                };

            <<"get_emotions">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_emotions',
                    params          =   Params
                };
            <<"get_emotion">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_emotions',
                    params          =   Params
                };

            <<"register">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'register',
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   User_id,
                    pers_perm_names =   User_perm_names
                };
            %%
            %% {"fname": "login", "params":{"nick":"admin", "pass":"admin"}} 
            %%
            <<"login">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   login,
                    params          =   Params
                };

            %%
            %% {"fname": "logout", "params":{"nick":"admin"}}
            %%
            <<"logout">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   logout,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   User_id,
                    pers_perm_names =   User_perm_names
                };

            %%
            %% {"fname": "update_pers", "params":{"id":1,"hobby":"some a"}}
            %%
            <<"update_pers">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   update_pers,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   User_id,
                    pers_perm_names =   User_perm_names
                };

            %%
            %% {"fname": "get_pers", "params":{"nick":"admin"}}
            %%
            <<"get_pers">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   get_pers,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   User_id,
                    pers_perm_names =   User_perm_names
                };

            %%
            %% {"fname": "get_all_perss", "params":{}}
            %%
            <<"get_all_perss">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   get_all_perss,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   User_id,
                    pers_perm_names =   User_perm_names
                };

            %%
            %% {"fname": "get_friends", "params":{"pers_id":1}}
            %%
            <<"get_friends">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   get_friends,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   User_id,
                    pers_perm_names =   User_perm_names
                };

            %%
            %%  {
            %%      "fname": "add_friend",
            %%      "params":{"pers_id":1, "friend_id":1}
            %%  }
            %%
            <<"add_friend">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   add_friend,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   User_id,
                    pers_perm_names =   User_perm_names
                };

            %%
            %%  {
            %%      "fname": "delete_friend",
            %%      "params":{"pers_id":1, "friend_id":1}
            %%  } 
            %%
            <<"delete_friend">> ->  
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   delete_friend,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   User_id,
                    pers_perm_names =   User_perm_names
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

