%% Feel free to use, reuse and abuse the code in this file.

-module(handler_debug_tests).
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



init(Mod, Req, Options) ->
    {Path_info, Req2} = cowboy_http_req:path_info(Req),
    init(Mod, Req2, Options, Path_info).


init(_, Req, _, Path_info) ->
    Aobj = empweb_http:auth(Req),
    Is_auth=            biz_user:is_auth(Aobj),
    User_id=            biz_user:get_user_id(Aobj),
    User_perm_names=    biz_user:get_perm_names(Aobj),
    Hap = case Path_info of
        [<<"jsonapi_user">>, <<"get_all_users">>] ->
                #empweb_hap{
                    handler         =   jsonapi_user,
                    action          =   get_all_users,
                    params          =   [],
                    is_auth         =   Is_auth,
                    user_id         =   User_id,
                    user_perm_names =   User_perm_names
                };
        _ ->
            []
    end,
    {ok, Req, Hap}.

handle(Req, #empweb_hap{}=Hap) ->
    make_reply(Req, handle_hap(Req, Hap));

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

handle_hap(Req, #empweb_hap{handler=jsonapi_user,action=get_all_users}=Hap)->
    Empweb_resp = case empweb_http:call(Req, Hap) of
        {ok, R} -> R;
        {error, unknown_function} ->
            jsonapi:not_extended(unknown_function);
        {error, Error} ->
            jsonapi:internal_server_error(
                {[{unknown_error, jsonapi:format(Error)}]}
            )
    end,
    {ok, Empweb_resp, Hap}.

make_reply(Req, {ok, Empweb_resp, Hap}) ->
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
    {ok,Reply,Hap}.

terminate(_Req, _State) ->
    ok.
