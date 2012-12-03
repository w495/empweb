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

-record(state, {
    empweb_hap
}).


init(_, Req, _Opts) ->
    ?evman_warning({erlang:time(), Req}),
    {Auth, Req1}    =   empweb_http:auth(Req),
    Is_auth         =   empweb_biz_pers:is_auth(Auth),
    Pid             =   empweb_biz_pers:get_pers_id(Auth),
    Pperm_names     =   empweb_biz_pers:get_perm_names(Auth),
    {ok, Req1, #state{
        empweb_hap  =
            #empweb_hap {
                auth            =   Auth,
                is_auth         =   Is_auth,
                pers_id         =   Pid,
                pers_perm_names =   Pperm_names
            }
            
    }}.

handle(Req, State) ->
    ?evman_args([Req, State]),
    {Empweb_resp, Reqres}  =
        case empweb_http:method(Req) of
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

    % io:format("Http_resp_json = ~p ~n~n~n", [Http_resp_json]),
    
    Reply =
        empweb_http:reply(
            Http_resp#http_resp{body = Http_resp_json},
            Reqres
        ),
    ?evman_debug({reply, Reply}, <<"server reply">>),
    {ok,Reply,State}.

handle_post(Req, State)->
    ?evman_args([Req, State]),
    case empweb_http:body_qs(Req) of
        {Pbody, Req1} ->
            handle_body(Req1, Pbody, State);
        _ ->
            {empweb_jsonapi:not_extended(no_post_body), Req}
    end.

handle_body(Req, Pbody, State)->
    ?evman_args([Req, Pbody, State]),

    case proplists:get_value(<<"data">>, Pbody) of
        undefined ->
            {empweb_jsonapi:not_extended(no_data), Req};
        Bobject ->
            handle_data(Req, Bobject, State)
    end.

handle_data(Req, Bobject, State)->
    ?evman_args([Req, Bobject, State]),
%     try
        ?evman_debug({bobject, Bobject},        <<"binary object">>),
        Object  =  ejson:decode(Bobject),
        ?evman_debug({object, Object},          <<"native object">>),
        {Res, Reqres}  =  empweb_jsonapi_map(Req, Object, State),
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

empweb_jsonapi_map(Req, {List}, State) ->
    ?evman_args([Req, List]),

    Fname   =  proplists:get_value(<<"fname">>, List),
    Params  =
        case proplists:get_value(<<"params">>, List, []) of
            null ->
                [];
            Res ->
                Res
        end,
    ?evman_debug({empweb_jsonapi_params, Params},  <<"empweb_jsonapi params">>),


    Eh =  State#state.empweb_hap#empweb_hap{
        params          =   Params
    },

    Hap =
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
            <<"count_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   count_room
                };
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
            <<"count_community">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   count_community
                };
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
            %%
            %% фото
            %%
            <<"get_photo">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_photo,
                    action          =   'get'
                };
            <<"get_all_photos">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_photo,
                    action          =   'get'
                };
            <<"create_photo">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_photo,
                    action          =   'create'
                };
            <<"update_photo">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_photo,
                    action          =   'update'
                };
            <<"delete_photo">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_photo,
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
            %% {"fname": "get_pers", "params":{"nick":"admin"}}
            %%
            <<"count_pers">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   count_pers
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
                    action          =   delete
                };
            
            %% ==================================================
            <<"get_roomlot">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   'get'
                };
            <<"get_all_roomlots">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   'get'
                };
            <<"create_roomlot">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   create
                };
            <<"update_roomlot">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   update
                };
            <<"delete_roomlot">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   delete
                };

            %% ==================================================
            <<"get_roombet">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   'get'
                };
            <<"get_all_roombets">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   'get'
                };
            <<"create_roombet">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   create
                };
            <<"update_roombet">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   update
                };
            <<"delete_roombet">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roombet,
                    action          =   delete
                };

            %% ==================================================
            <<"get_paytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   'get'
                };
            <<"get_all_paytypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   'get'
                };
            <<"create_paytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   create
                };
            <<"update_paytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   update
                };
            <<"delete_paytype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_paytype,
                    action          =   delete
                };

            %% ==================================================
            <<"get_pay">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   'get'
                };
            <<"get_all_pays">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   'get'
                };
            <<"create_pay">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   create
                };
            <<"update_pay">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   update
                };
            <<"delete_pay">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pay,
                    action          =   delete
                };

            %% ==================================================
            <<"get_treastype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   'get'
                };
            <<"get_all_treastypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   'get'
                };
            <<"create_treastype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   create
                };
            <<"update_treastype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   update
                };
            <<"delete_treastype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_treastype,
                    action          =   delete
                };

            %% ==================================================
            <<"get_roomtreas">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   'get'
                };
            <<"get_all_roomtreass">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   'get'
                };
            <<"create_roomtreas">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   create
                };
            <<"update_roomtreas">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   update
                };
            <<"delete_roomtreas">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomtreas,
                    action          =   delete
                };

            %% ==================================================
            <<"get_transtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_transtype,
                    action          =   'get'
                };
            <<"get_all_transtypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_transtype,
                    action          =   'get'
                };
            <<"create_transtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_transtype,
                    action          =   create
                };
            <<"update_transtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_transtype,
                    action          =   update
                };
            <<"delete_transtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_transtype,
                    action          =   delete
                };
                
            %% ==================================================
            <<"get_rptrans">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_rptrans,
                    action          =   'get'
                };
            <<"get_all_rptranss">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_rptrans,
                    action          =   'get'
                };
            <<"create_rptrans">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_rptrans,
                    action          =   create
                };
            <<"update_rptrans">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_rptrans,
                    action          =   update
                };
            <<"delete_rptrans">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_rptrans,
                    action          =   delete
                };
                
            %% ==================================================
            <<"get_communitymemb">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitymemb,
                    action          =   'get'
                };
            <<"get_all_communitymembs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitymemb,
                    action          =   'get'
                };
            <<"create_communitymemb">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitymemb,
                    action          =   create
                };
            <<"update_communitymemb">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitymemb,
                    action          =   update
                };
            <<"delete_communitymemb">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitymemb,
                    action          =   delete
                };

            %% ==================================================
            <<"get_communitycand">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitycand,
                    action          =   'get'
                };
            <<"get_all_communitycands">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitycand,
                    action          =   'get'
                };
            <<"create_communitycand">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitycand,
                    action          =   create
                };
            <<"update_communitycand">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitycand,
                    action          =   update
                };
            <<"delete_communitycand">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitycand,
                    action          =   delete
                };

            %% ==================================================
            <<"get_geo">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_geo,
                    action          =   'get'
                };
            <<"get_all_geos">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_geo,
                    action          =   'get'
                };
            <<"create_geo">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_geo,
                    action          =   create
                };
            <<"update_geo">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_geo,
                    action          =   update
                };
            <<"delete_geo">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_geo,
                    action          =   delete
                };


            %% ==================================================
            <<"get_noticetype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_noticetype,
                    action          =   'get'
                };
            <<"get_all_noticetypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_noticetype,
                    action          =   'get'
                };
            <<"create_noticetype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_noticetype,
                    action          =   create
                };
            <<"update_noticetype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_noticetype,
                    action          =   update
                };
            <<"delete_noticetype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_noticetype,
                    action          =   delete
                };

        %% ==================================================
            <<"get_notice">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_notice,
                    action          =   'get'
                };
            <<"get_all_notices">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_notice,
                    action          =   'get'
                };
            <<"create_notice">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_notice,
                    action          =   create
                };
            <<"update_notice">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_notice,
                    action          =   update
                };
            <<"delete_notice">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_notice,
                    action          =   delete
                };


        %% ==================================================
            <<"get_communityhist">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communityhist,
                    action          =   'get'
                };
            <<"get_all_communityhists">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communityhist,
                    action          =   'get'
                };
            <<"create_communityhist">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communityhist,
                    action          =   create
                };
            <<"update_communityhist">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communityhist,
                    action          =   update
                };
            <<"delete_communityhist">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communityhist,
                    action          =   delete
                };


        %% ==================================================
            <<"get_communityhisttype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communityhisttype,
                    action          =   'get'
                };
            <<"get_all_communityhisttypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communityhisttype,
                    action          =   'get'
                };
            <<"create_communityhisttype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communityhisttype,
                    action          =   create
                };
            <<"update_communityhisttype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communityhisttype,
                    action          =   update
                };
            <<"delete_communityhisttype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communityhisttype,
                    action          =   delete
                };
                
            _ -> []
        end,
    ?evman_debug({empweb_jsonapi_action, Hap}),
    empweb_jsonapi:call(Req, Hap, Fname);

empweb_jsonapi_map(Req, [{_}|_] = List, State) ->
    ?evman_args([List]),
    {Res, Reqres} =
        lists:foldl(
            fun(Item, {Acc, Reqi})->
                {Resi, Reqresi} = empweb_jsonapi_map(Reqi, Item, State),
                {[Resi|Acc], Reqresi}
            end,
            {[], Req},
            List
        ),
    %io:format("Res = ~p~n~n~n", [Res]),
    {lists:reverse(Res), Reqres};

empweb_jsonapi_map(Req, List, State) ->
    ?evman_args([List]),
    {empweb_jsonapi:not_extended(wrong_format), Req}.
