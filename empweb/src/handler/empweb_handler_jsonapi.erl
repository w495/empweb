%% Feel free to use, reuse and abuse the code in this file.

-module(empweb_handler_jsonapi).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

%%
%% Определения общие для всего приложения
%%
-include("empweb.hrl").

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
    ?evman_warning({erlang:time(), Req}),
    {ok, Req, undefined_state}.

handle(Req, State) ->
    ?evman_args([Req, State]),
    {Empweb_resp, Reqres}  =
        case cowboy_http_req:method(Req) of
            {'POST', Req1} ->
                handle_post(Req1, State);
            {_, Req1} ->
                {empweb_jsonapi:method_not_allowed(), Req1}
        end,
    ?evman_debug({empweb_resp, Empweb_resp}, <<"empweb response">>),
    Http_resp = empweb_http:resp(Empweb_resp),
    ?evman_debug({http_resp, Http_resp}, <<"http response">>),
    Http_resp_json = ejson:encode(Http_resp#http_resp.body),
    ?evman_debug({http_resp_json, Http_resp_json}, <<"http json">>),
    {ok, Reply} = cowboy_http_req:reply(
        Http_resp#http_resp.status,
        Http_resp#http_resp.headers,
        %[{<<"Connection">>,<<"close">>}|Http_resp#http_resp.headers],
        Http_resp_json,
        Reqres
    ),

    ?evman_debug({reply, Reply}, <<"server reply">>),

    {ok,Reply,State}.

handle_post(Req, State)->
    ?evman_args([Req, State]),
    case cowboy_http_req:body_qs(Req) of
        {Pbody, Req1} ->
            handle_post_body(Req1, State, Pbody);
        _ ->
            {empweb_jsonapi:not_extended(no_post_body), Req}
    end.

handle_post_body(Req, State, Pbody)->
    ?evman_args([Req, State, Pbody]),

    case proplists:get_value(<<"data">>, Pbody) of
        undefined ->
            {empweb_jsonapi:not_extended(no_data), Req};
        Bobject ->
            handle_data(Req, State, Bobject)
    end.

handle_data(Req, State, Bobject)->
    ?evman_args([Req, State, Bobject]),
%     try
        ?evman_debug({bobject, Bobject},        <<"binary object">>),
        Object  =  ejson:decode(Bobject),
        ?evman_debug({object, Object},          <<"native object">>),
        {Res, Reqres}  =  empweb_jsonapi_map(Req, Object),
        ?evman_debug({empweb_jsonapi_result, Res},  <<"empweb_jsonapi result">>),
        {Res, Reqres}
%     catch
%         throw:{invalid_json, _} ->
%             {empweb_jsonapi:not_extended(invalid_json), Req};
%         Eclass:Ereason ->
%             ?evman_error(#event{error={Eclass,Ereason}}),
%             {empweb_jsonapi:internal_server_error(
%                 {[
%                     {unknown_error,
%                         {[
%                             {class, empweb_jsonapi:format(Eclass)},
%                             {reason, empweb_jsonapi:format(Ereason)}
%                         ]}
%                     }
%                 ]}
%             ), Req}
%     end
    .

terminate(Req, State) ->
    ?evman_args([Req, State]),
    ?evman_warning(Req),
    ok.

empweb_jsonapi_map(Req, {List}) ->
    ?evman_args([Req, List]),

    Fname   =  proplists:get_value(<<"fname">>, List),
    Params  =  case proplists:get_value(<<"params">>, List, []) of
        null ->
            [];
        Res ->
            Res
    end,
    ?evman_debug({empweb_jsonapi_params, Params},  <<"empweb_jsonapi params">>),
    {Auth, Req1} = empweb_http:auth(Req),

    io:format("Auth = ~p~n", [Auth]),

    ?evman_debug({empweb_jsonapi_aobj, Auth},  <<"empweb_jsonapi aobj">>),

    Is_auth=            empweb_biz_pers:is_auth(Auth),

    io:format("Is_auth = ~p~n", [Is_auth]),

    ?evman_debug({empweb_jsonapi_aobj, Auth},  <<"empweb_jsonapi aobj">>),

    Pid         =   empweb_biz_pers:get_pers_id(Auth),
    Pperm_names =   empweb_biz_pers:get_perm_names(Auth),

    ?evman_debug({empweb_jsonapi_call, [
        {fname,     Fname},
        {params,    Params},
        {is_auth,   Is_auth}
    ]}),

    Action =
        case Fname of
            %%
            %% Тип разрешения: не рассмотрен, запрещена, разрешена
            %%
            <<"get_oktype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_oktype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_oktypes">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_oktype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_oktype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_oktype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_oktype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_oktype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_oktype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_oktype,
                    pers_id         =   Pid,
                    params          =   Params
                };
        %% ----------------------------------------------------------------
            <<"get_thingtype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thingtype,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_thingtypes">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thingtype,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_thingtype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thingtype,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_thingtype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thingtype,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_thingtype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thingtype,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };
                
            <<"get_thing">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thing,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_things">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thing,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_thing">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thing,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_thing">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thing,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_thing">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thing,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };
        %% ----------------------------------------------------------------
        %% Функции документов
        %% ----------------------------------------------------------------

            %%
            %% Тип документа: Блог, коммент к блогу, галерея,
            %%      фото, коммент к фото, attach descr.
            %%
            <<"get_doctype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_doctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_doctypes">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_doctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_doctype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_doctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_doctype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_doctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_doctype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_doctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Типы контента: обычный, эротический
            %%
            <<"get_contype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_contype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_contypes">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_contype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_contype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_contype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_contype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_contype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_contype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_contype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Тип доступа к контенту контента (блога и галереи):
            %%  приватный, дружеский, открытый.
            %%
            <<"get_acctype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_acctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_acctypes">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_acctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_acctype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_acctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_acctype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_acctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_acctype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_acctype,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %%
            %% Типы чат-комнат. (страна, тюрьма, ад, рай)
            %%
            <<"get_roomtype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_roomtype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_roomtypes">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_roomtype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_roomtype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_roomtype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_roomtype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_roomtype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_roomtype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_roomtype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Список языков чата.
            %%
            <<"get_chatlang">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_chatlang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_chatlangs">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_chatlang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_chatlang">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_chatlang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_chatlang">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_chatlang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_chatlang">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_chatlang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Список режимов страны
            %%
            <<"get_regimen">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_regimen,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_regimens">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_regimen,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_regimen">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_regimen,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_regimen">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_regimen,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_regimen">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_regimen,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Дерево тем страны
            %%
            <<"get_topic">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_topic,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_topics">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_topic,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_topic">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_topic,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_topic">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_topic,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_topic">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_topic,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Типы сообществ (обычные, тайные)
            %%
            <<"get_communitytype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_communitytype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_communitytypes">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_communitytype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_communitytype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_communitytype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_communitytype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_communitytype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_communitytype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_communitytype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Блоги
            %%
            <<"get_blog">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_blog,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_blogs">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_blog,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_blog">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_blog,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_blog">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_blog,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_blog">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_blog,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Посты 
            %%
            <<"get_post">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_post,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_posts">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_post,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_post">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_post,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_post">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_post,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_post">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_post,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Kоменты
            %%
            <<"get_comment">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_comment,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_comments">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_comment,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_comment">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_comment,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_comment">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_comment,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_comment">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_comment,
                    pers_id         =   Pid,
                    params          =   Params
                };  
            %%
            %% Чат-комнаты (страны)
            %%
            <<"get_room">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_room,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_rooms">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_room,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_room">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_room,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_room">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_room,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_room">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_room,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"add_room_topic">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   add_room_topic,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_room_topic">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_room_topic,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Сообщества
            %%
            <<"get_community">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_community,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_communities">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_community,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_community">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_community,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_community">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_community,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_community">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_community,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %%
            %% Сообщения
            %%
            <<"count_message">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   count_message,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"count_message_types">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   count_message_types,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_message">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_message,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_message_for_me">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_message_for_me,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_message_from_me">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_message_from_me,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_messages">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_message,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_message">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_message,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_message">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_message,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_message_for_me">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_message_for_me,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_message_from_me">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_message_from_me,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_message">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_message,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Альбомы
            %%
            <<"get_album">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_album,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_albums">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_album,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_album">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_album,
                    action          =   'create',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_album">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_album,
                    action          =   'update',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_album">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_album,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };
        %% ----------------------------------------------------------------
        %% Функции системы
        %% ----------------------------------------------------------------

            %%
            %% Список языков системы.
            %%
            <<"get_lang">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   get_lang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_langs">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   get_lang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_lang">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   create_lang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_lang">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   update_lang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_lang">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   delete_lang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Список переводов системы.
            %%
            <<"get_tr">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   get_tr,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_trs">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   get_tr,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_tr">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   create_tr,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_tr">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   update_tr,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_tr">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   delete_tr,
                    pers_id         =   Pid,
                    params          =   Params
                };

        %% ----------------------------------------------------------------
        %% Функции пользователей
        %% ----------------------------------------------------------------

            <<"get_all_authorities">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_authority,
                    params          =   Params
                };
            <<"get_authority">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_authority,
                    params          =   Params
                };

            <<"get_all_mstatuses">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_mstatus',
                    params          =   Params
                };
            <<"get_mstatus">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_mstatus',
                    params          =   Params
                };

            <<"get_all_pstatuses">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_pstatus',
                    params          =   Params
                };
            <<"get_pstatus">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_pstatus',
                    params          =   Params
                };

            <<"get_all_emotions">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_emotion',
                    params          =   Params
                };
            <<"get_emotion">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_emotion',
                    params          =   Params
                };

            <<"register">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'register',
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
                };
            %%
            %% {"fname": "login", "params":{"nick":"admin", "pass":"admin"}} 
            %%
            <<"login">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   login,
                    params          =   Params
                };
            %%
            %% {"fname": "login", "params":{"nick":"admin", "pass":"admin"}}
            %%
            <<"pass">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   pass,
                    params          =   Params,
                    auth            =   Auth,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
                };

            %%
            %% {"fname": "logout", "params":{"nick":"admin"}}
            %%
            <<"logout">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   logout,
                    params          =   Params,
                    auth            =   Auth,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
                };

            %%
            %% {"fname": "update_pers", "params":{"id":1,"hobby":"some a"}}
            %%
            <<"update_pers">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   update_pers,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
                };

            %%
            %% {"fname": "get_pers", "params":{"nick":"admin"}}
            %%
            <<"get_pers">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_pers,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
                };

            %%
            %% {"fname": "get_all_perss", "params":{}}
            %%
            <<"get_all_perss">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_pers,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
                };

            %%
            %% {"fname": "get_friend", "params":{"pers_id":1}}
            %%
            <<"get_friend">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_friend,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
                };
            <<"get_friends">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_friend,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
                };

            %%
            %%  {
            %%      "fname": "add_friend",
            %%      "params":{"pers_id":1, "friend_id":1}
            %%  }
            %%
            <<"add_friend">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   add_friend,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
                };

            %%
            %%  {
            %%      "fname": "delete_friend",
            %%      "params":{"pers_id":1, "friend_id":1}
            %%  } 
            %%
            <<"delete_friend">> ->  
                #empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   delete_friend,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
                };

            %% ==================================================

            <<"get_thingbuy">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_thingbuys">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_thingbuy">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_thingbuy">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_thingbuy">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            <<"buy_product">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %% ==================================================
            
            <<"get_experbuy">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_experbuy,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_experbuys">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_experbuy,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_experbuy">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_experbuy,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_experbuy">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_experbuy,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_experbuy">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_experbuy,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };
            
            %% ==================================================
            <<"get_roomlot">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_roomlots">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_roomlot">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_roomlot">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_roomlot">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %% ==================================================
            <<"get_roombet">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_roombets">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_roombet">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_roombet">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_roombet">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %% ==================================================
            <<"get_paytype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_paytypes">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_paytype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_paytype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_paytype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %% ==================================================
            <<"get_pay">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_pays">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_pay">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_pay">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_pay">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %% ==================================================
            <<"get_treastype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_treastypes">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_treastype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_treastype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_treastype">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %% ==================================================
            <<"get_roomtreas">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_roomtreass">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_roomtreas">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_roomtreas">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_roomtreas">> ->
                #empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            _ -> []
        end,

    ?evman_debug({empweb_jsonapi_action, Action}),

%     ?debug("01-=-----------------------------------------------~n"),
%     case empweb_http:call(Req1, Action) of
%         {ok, Reply} ->
%             ?debug("00-=-----------------------------------------------~n"),
%             {Reply, Req1};
%         {error, unknown_function} ->
%             ?debug("000-=-----------------------------------------------~n"),
%             {empweb_jsonapi:not_extended(unknown_function), Req1};
%         {error, Error} ->
%             ?debug("0000-=-----------------------------------------------~n"),
%             {empweb_jsonapi:internal_server_error(
%                 {[{unknown_error1, empweb_jsonapi:format(Error)}]}
%             ), Req1};
%         X ->
%             ?debug("0000000-=-----------------------------------------------~n")
%     end;

    empweb_jsonapi_call(Req1, Action, Fname);

empweb_jsonapi_map(Req, List) ->
    ?evman_args([List]),
    {empweb_jsonapi:not_extended(wrong_format), Req}.

empweb_jsonapi_call(Req1, Action, Fname) ->
    {Res, Req} = case empweb_http:call(Req1, Action) of
        {ok, Reply} ->
            {Reply, Req1};
        {error, unknown_function} ->
            {empweb_jsonapi:not_extended(unknown_function), Req1};
        {error, Error} ->
            {empweb_jsonapi:internal_server_error(
                {[{unknown_error1, empweb_jsonapi:format(Error)}]}
            ), Req1}
    end,
    {Rpl} = Res#empweb_resp.body,
    %{Res#empweb_resp{body = {[{fname, Action#empweb_hap.action}|Rpl]}}, Req}.
    {Res#empweb_resp{body = {[{fname, Fname}|Rpl]}}, Req}.
