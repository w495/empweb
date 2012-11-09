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


    Eh = #empweb_hap{
        params          =   Params,
        auth            =   Auth,
        is_auth         =   Is_auth,
        pers_id         =   Pid,
        pers_perm_names =   Pperm_names
    },
    
    Action =
        case Fname of
            %%
            %% Тип разрешения: не рассмотрен, запрещена, разрешена
            %%
            <<"get_oktype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_oktype
                };
            <<"get_all_oktypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_oktype
                };
            <<"create_oktype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_oktype
                };
            <<"update_oktype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_oktype
                };
            <<"delete_oktype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_oktype
                };
        %% ----------------------------------------------------------------
            <<"get_thingtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingtype,
                    action          =   'get'
                };
            <<"get_all_thingtypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingtype,
                    action          =   'get'
                };
            <<"create_thingtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingtype,
                    action          =   create
                };
            <<"update_thingtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingtype,
                    action          =   update
                };
            <<"delete_thingtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingtype,
                    action          =   delete
                };
                
            <<"get_thing">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thing,
                    action          =   'get'
                };
            <<"get_all_things">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thing,
                    action          =   'get'
                };
            <<"create_thing">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thing,
                    action          =   create
                };
            <<"update_thing">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thing,
                    action          =   update
                };
            <<"delete_thing">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thing,
                    action          =   delete
                };
        %% ----------------------------------------------------------------
        %% Функции документов
        %% ----------------------------------------------------------------

            %%
            %% Тип документа: Блог, коммент к блогу, галерея,
            %%      фото, коммент к фото, attach descr.
            %%
            <<"get_doctype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_doctype
                };
            <<"get_all_doctypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_doctype
                };
            <<"create_doctype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_doctype
                };
            <<"update_doctype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_doctype
                };
            <<"delete_doctype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_doctype
                };
            %%
            %% Типы контента: обычный, эротический
            %%
            <<"get_contype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_contype
                };
            <<"get_all_contypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_contype
                };
            <<"create_contype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_contype
                };
            <<"update_contype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_contype
                };
            <<"delete_contype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_contype
                };
            %%
            %% Тип доступа к контенту контента (блога и галереи):
            %%  приватный, дружеский, открытый.
            %%
            <<"get_acctype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_acctype
                };
            <<"get_all_acctypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_acctype
                };
            <<"create_acctype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_acctype
                };
            <<"update_acctype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_acctype
                };
            <<"delete_acctype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_acctype
                };

            %%
            %% Типы чат-комнат. (страна, тюрьма, ад, рай)
            %%
            <<"get_roomtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_roomtype
                };
            <<"get_all_roomtypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_roomtype
                };
            <<"create_roomtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_roomtype
                };
            <<"update_roomtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_roomtype
                };
            <<"delete_roomtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_roomtype
                };
            %%
            %% Список языков чата.
            %%
            <<"get_chatlang">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_chatlang
                };
            <<"get_all_chatlangs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_chatlang
                };
            <<"create_chatlang">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_chatlang
                };
            <<"update_chatlang">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_chatlang
                };
            <<"delete_chatlang">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_chatlang
                };
            %%
            %% Список режимов страны
            %%
            <<"get_regimen">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_regimen
                };
            <<"get_all_regimens">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_regimen
                };
            <<"create_regimen">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_regimen
                };
            <<"update_regimen">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_regimen
                };
            <<"delete_regimen">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_regimen
                };
            %%
            %% Дерево тем страны
            %%
            <<"get_topic">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_topic
                };
            <<"get_all_topics">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_topic
                };
            <<"create_topic">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_topic
                };
            <<"update_topic">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_topic
                };
            <<"delete_topic">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_topic
                };
            %%
            %% Типы сообществ (обычные, тайные)
            %%
            <<"get_communitytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_communitytype
                };
            <<"get_all_communitytypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_communitytype
                };
            <<"create_communitytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_communitytype
                };
            <<"update_communitytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_communitytype
                };
            <<"delete_communitytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_communitytype
                };
            %%
            %% Блоги
            %%
            <<"get_blog">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_blog
                };
            <<"get_all_blogs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_blog
                };
            <<"create_blog">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_blog
                };
            <<"update_blog">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_blog
                };
            <<"delete_blog">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_blog
                };
            %%
            %% Посты 
            %%
            <<"get_post">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_post
                };
            <<"get_all_posts">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_post
                };
            <<"create_post">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_post
                };
            <<"update_post">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_post
                };
            <<"delete_post">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_post
                };
            %%
            %% Kоменты
            %%
            <<"get_comment">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_comment
                };
            <<"get_all_comments">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_comment
                };
            <<"create_comment">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_comment
                };
            <<"update_comment">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_comment
                };
            <<"delete_comment">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_comment
                };  
            %%
            %% Чат-комнаты (страны)
            %%
            <<"get_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_room
                };
            <<"join_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   join_room
                };
            <<"get_all_rooms">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_room
                };
            <<"create_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_room
                };
            <<"update_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_room
                };
            <<"delete_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_room
                };
            <<"add_room_topic">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   add_room_topic
                };
            <<"delete_room_topic">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_room_topic
                };
            %%
            %% Сообщества
            %%
            <<"get_community">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_community
                };
            <<"get_all_communities">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_community
                };
            <<"create_community">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_community
                };
            <<"update_community">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_community
                };
            <<"delete_community">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_community
                };

            %%
            %% Сообщения
            %%
            <<"count_message">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   count_message
                };
            <<"count_message_types">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   count_message_types
                };
            <<"get_message">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_message
                };
            <<"get_message_for_me">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_message_for_me
                };
            <<"get_message_from_me">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_message_from_me
                };
            <<"get_all_messages">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_message
                };
            <<"create_message">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   create_message
                };
            <<"update_message">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   update_message
                };
            <<"delete_message_for_me">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_message_for_me
                };
            <<"delete_message_from_me">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_message_from_me
                };
            <<"delete_message">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   delete_message
                };
            %%
            %% Альбомы
            %%
            <<"get_album">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_album,
                    action          =   'get'
                };
            <<"get_all_albums">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_album,
                    action          =   'get'
                };
            <<"create_album">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_album,
                    action          =   'create'
                };
            <<"update_album">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_album,
                    action          =   'update'
                };
            <<"delete_album">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_album,
                    action          =   delete
                };
        %% ----------------------------------------------------------------
        %% Функции системы
        %% ----------------------------------------------------------------

            %%
            %% Список языков системы.
            %%
            <<"get_lang">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   get_lang
                };
            <<"get_all_langs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   get_lang
                };
            <<"create_lang">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   create_lang
                };
            <<"update_lang">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   update_lang
                };
            <<"delete_lang">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   delete_lang
                };
            %%
            %% Список переводов системы.
            %%
            <<"get_tr">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   get_tr
                };
            <<"get_all_trs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   get_tr
                };
            <<"create_tr">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   create_tr
                };
            <<"update_tr">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   update_tr
                };
            <<"delete_tr">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_conf,
                    action          =   delete_tr
                };

        %% ----------------------------------------------------------------
        %% Функции пользователей
        %% ----------------------------------------------------------------

            <<"get_all_authorities">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_authority
                };
            <<"get_authority">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_authority
                };

            <<"get_all_mstatuses">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_mstatus'
                };
            <<"get_mstatus">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_mstatus'
                };

            <<"get_all_pstatuses">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_pstatus'
                };
            <<"get_pstatus">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_pstatus'
                };

            <<"get_all_emotions">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_emotion'
                };
            <<"get_emotion">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_emotion'
                };

            <<"register">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'register'
                };
            %%
            %% {"fname": "login", "params":{"nick":"admin", "pass":"admin"}} 
            %%
            <<"login">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   login
                };
            %%
            %% {"fname": "login", "params":{"nick":"admin", "pass":"admin"}}
            %%
            <<"pass">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   pass
                };

            %%
            %% {"fname": "logout", "params":{"nick":"admin"}}
            %%
            <<"logout">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   logout
                };

            %%
            %% {"fname": "update_pers", "params":{"id":1,"hobby":"some a"}}
            %%
            <<"update_pers">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   update_pers
                };

            %%
            %% {"fname": "get_pers", "params":{"nick":"admin"}}
            %%
            <<"get_pers">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_pers
                };

            %%
            %% {"fname": "get_all_perss", "params":{}}
            %%
            <<"get_all_perss">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_pers
                };

            %%
            %% {"fname": "get_friend", "params":{"pers_id":1}}
            %%
            <<"get_friend">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_friend
                };
            <<"get_friends">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   get_friend
                };

            %%
            %%  {
            %%      "fname": "add_friend",
            %%      "params":{"pers_id":1, "friend_id":1}
            %%  }
            %%
            <<"add_friend">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   add_friend
                };

            %%
            %%  {
            %%      "fname": "delete_friend",
            %%      "params":{"pers_id":1, "friend_id":1}
            %%  } 
            %%
            <<"delete_friend">> ->  
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   delete_friend
                };

            %% ==================================================

            <<"get_thingbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   'get'
                };
            <<"get_all_thingbuys">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   'get'
                };
            <<"create_thingbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   create
                };
            <<"update_thingbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   update
                };
            <<"delete_thingbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   delete
                };

            <<"buy_product">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   create
                };

            %% ==================================================
            
            <<"get_experbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_experbuy,
                    action          =   'get'
                };
            <<"get_all_experbuys">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_experbuy,
                    action          =   'get'
                };
            <<"create_experbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_experbuy,
                    action          =   create
                };
            <<"update_experbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_experbuy,
                    action          =   update
                };
            <<"delete_experbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_experbuy,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };
            
            %% ==================================================
            <<"get_roomlot">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_roomlots">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_roomlot">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_roomlot">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_roomlot">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %% ==================================================
            <<"get_roombet">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_roombets">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_roombet">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_roombet">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_roombet">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %% ==================================================
            <<"get_paytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_paytypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_paytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_paytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_paytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %% ==================================================
            <<"get_pay">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_pays">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_pay">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_pay">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_pay">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %% ==================================================
            <<"get_treastype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_treastypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_treastype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_treastype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_treastype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   delete,
                    pers_id         =   Pid,
                    params          =   Params
                };

            %% ==================================================
            <<"get_roomtreas">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_roomtreass">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   'get',
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_roomtreas">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   create,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_roomtreas">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   update,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_roomtreas">> ->
                Eh#empweb_hap{
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
