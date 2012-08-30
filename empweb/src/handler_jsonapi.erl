%% Feel free to use, reuse and abuse the code in this file.

-module(handler_jsonapi).
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
                {jsonapi:method_not_allowed(), Req1}
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
            {jsonapi:not_extended(no_post_body), Req}
    end.

handle_post_body(Req, State, Pbody)->
    ?evman_args([Req, State, Pbody]),

    case proplists:get_value(<<"data">>, Pbody) of
        undefined ->
            {jsonapi:not_extended(no_data), Req};
        Bobject ->
            handle_data(Req, State, Bobject)
    end.

handle_data(Req, State, Bobject)->
    ?evman_args([Req, State, Bobject]),
    
    try
        ?evman_debug({bobject, Bobject},        <<"binary object">>),
        Object  =  ejson:decode(Bobject),
        ?evman_debug({object, Object},          <<"native object">>),
        {Res, Reqres}  =  jsonapi_map(Req, Object),
        ?evman_debug({jsonapi_result, Res},  <<"jsonapi result">>),
        {Res, Reqres}
    catch
        throw:{invalid_json, _} ->
            {jsonapi:not_extended(invalid_json), Req};
        Eclass:Ereason ->
            ?evman_error(#event{error={Eclass,Ereason}}),
            {jsonapi:internal_server_error(
                {[
                    {unknown_error, 
                        {[
                            {class, jsonapi:format(Eclass)},
                            {reason, jsonapi:format(Ereason)}
                        ]}
                    }
                ]}
            ), Req}
    end.

terminate(Req, State) ->
    ?evman_args([Req, State]),
    ?evman_warning(Req),
    ok.

jsonapi_map(Req, {List}) ->
    ?evman_args([Req, List]),

    Fname   =  proplists:get_value(<<"fname">>, List),
    Params  =  case proplists:get_value(<<"params">>, List, []) of
        null ->
            [];
        Res ->
            Res
    end,
    ?evman_debug({jsonapi_params, Params},  <<"jsonapi params">>),
    {Auth, Req1} = empweb_http:auth(Req),

    ?evman_debug({jsonapi_aobj, Auth},  <<"jsonapi aobj">>),

    Is_auth=            biz_pers:is_auth(Auth),
    
    ?evman_debug({jsonapi_aobj, Auth},  <<"jsonapi aobj">>),
    
    Pid         =   biz_pers:get_pers_id(Auth),
    Pperm_names =   biz_pers:get_perm_names(Auth),

    ?evman_debug({jsonapi_call, [
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
                    handler         =   jsonapi_doc,
                    action          =   get_oktype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_oktypes">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_oktypes,
                    pers_id         =   Pid
                };
            <<"create_oktype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_oktype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_oktype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_oktype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_oktype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   delete_oktype,
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
                    handler         =   jsonapi_doc,
                    action          =   get_doctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_doctypes">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_doctypes,
                    pers_id         =   Pid
                };
            <<"create_doctype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_doctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_doctype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_doctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_doctype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   delete_doctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Типы контента: обычный, эротический
            %%
            <<"get_contype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_contype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_contypes">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_contypes,
                    pers_id         =   Pid
                };
            <<"create_contype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_contype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_contype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_contype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_contype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
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
                    handler         =   jsonapi_doc,
                    action          =   get_acctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_acctypes">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_acctypes,
                    pers_id         =   Pid
                };
            <<"create_acctype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_acctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_acctype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_acctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_acctype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   delete_acctype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Типы сообществ (обычные, тайные)
            %%
            <<"get_communitytype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_communitytype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_communitytypes">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_communitytypes,
                    pers_id         =   Pid
                };
            <<"create_communitytype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_communitytype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_communitytype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_communitytype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_communitytype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   delete_communitytype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Типы чат-комнат. (страна, тюрьма, ад, рай)
            %%
            <<"get_roomtype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_roomtype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_roomtypes">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_roomtypes,
                    pers_id         =   Pid
                };
            <<"create_roomtype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_roomtype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_roomtype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_roomtype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_roomtype">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   delete_roomtype,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Список языков чата.
            %%
            <<"get_chatlang">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_chatlang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_chatlangs">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_chatlangs,
                    pers_id         =   Pid
                };
            <<"create_chatlang">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_chatlang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_chatlang">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_chatlang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_chatlang">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   delete_chatlang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Список режимов страны
            %%
            <<"get_regimen">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_regimen,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_regimens">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_regimens,
                    pers_id         =   Pid
                };
            <<"create_regimen">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_regimen,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_regimen">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_regimen,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_regimen">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   delete_regimen,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Блоги
            %%
            <<"get_blog">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_blog,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_blogs">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_blogs,
                    pers_id         =   Pid
                };
            <<"create_blog">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_blog,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_blog">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_blog,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_blog">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   delete_blog,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Посты \ коменты
            %%
            <<"get_post">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_post,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_posts">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_posts,
                    pers_id         =   Pid
                };
            <<"create_post">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_post,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_post">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_post,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_post">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   delete_post,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Чат-комнаты (страны)
            %%
            <<"get_room">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_room,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_rooms">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_rooms,
                    pers_id         =   Pid
                };
            <<"create_room">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_room,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_room">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_room,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_room">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   delete_room,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Сообщества
            %%
            <<"get_community">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_community,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_communities">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   get_all_communities,
                    pers_id         =   Pid
                };
            <<"create_community">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   create_community,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_community">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   update_community,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_community">> ->
                #empweb_hap{
                    handler         =   jsonapi_doc,
                    action          =   delete_community,
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
                    handler         =   jsonapi_conf,
                    action          =   get_lang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_langs">> ->
                #empweb_hap{
                    handler         =   jsonapi_conf,
                    action          =   get_all_langs,
                    pers_id         =   Pid
                };
            <<"create_lang">> ->
                #empweb_hap{
                    handler         =   jsonapi_conf,
                    action          =   create_lang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_lang">> ->
                #empweb_hap{
                    handler         =   jsonapi_conf,
                    action          =   update_lang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_lang">> ->
                #empweb_hap{
                    handler         =   jsonapi_conf,
                    action          =   delete_lang,
                    pers_id         =   Pid,
                    params          =   Params
                };
            %%
            %% Список переводов системы.
            %%
            <<"get_tr">> ->
                #empweb_hap{
                    handler         =   jsonapi_conf,
                    action          =   get_tr,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"get_all_trs">> ->
                #empweb_hap{
                    handler         =   jsonapi_conf,
                    action          =   get_all_trs,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"create_tr">> ->
                #empweb_hap{
                    handler         =   jsonapi_conf,
                    action          =   create_tr,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"update_tr">> ->
                #empweb_hap{
                    handler         =   jsonapi_conf,
                    action          =   update_tr,
                    pers_id         =   Pid,
                    params          =   Params
                };
            <<"delete_tr">> ->
                #empweb_hap{
                    handler         =   jsonapi_conf,
                    action          =   delete_tr,
                    pers_id         =   Pid,
                    params          =   Params
                };

        %% ----------------------------------------------------------------
        %% Функции пользователей
        %% ----------------------------------------------------------------

            <<"get_all_authorities">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_all_authorities'
                };
            <<"get_authority">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_authority',
                    params          =   Params
                };

            <<"get_all_mstatuses">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_all_mstatuses'
                };
            <<"get_mstatus">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_mstatus',
                    params          =   Params
                };

            <<"get_all_pstatuses">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_all_pstatuses'
                };
            <<"get_pstatus">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_pstatus',
                    params          =   Params
                };

            <<"get_all_emotions">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_all_emotions'
                };
            <<"get_emotion">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
                    action          =   'get_emotion',
                    params          =   Params
                };

            <<"register">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
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
                    handler         =   jsonapi_pers,
                    action          =   login,
                    params          =   Params
                };
            %%
            %% {"fname": "login", "params":{"nick":"admin", "pass":"admin"}}
            %%
            <<"pass">> ->
                #empweb_hap{
                    handler         =   jsonapi_pers,
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
                    handler         =   jsonapi_pers,
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
                    handler         =   jsonapi_pers,
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
                    handler         =   jsonapi_pers,
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
                    handler         =   jsonapi_pers,
                    action          =   get_all_perss,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
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
                    handler         =   jsonapi_pers,
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
                    handler         =   jsonapi_pers,
                    action          =   delete_friend,
                    params          =   Params,
                    is_auth         =   Is_auth,
                    pers_id         =   Pid,
                    pers_perm_names =   Pperm_names
                };

            _ -> []
        end,

    ?evman_debug({jsonapi_action, Action}),
    
    case empweb_http:call(Req1, Action) of
        {ok, Reply} ->
            {Reply, Req1};
        {error, unknown_function} ->
            {jsonapi:not_extended(unknown_function), Req1};
        {error, Error} ->
            {jsonapi:internal_server_error(
                {[{unknown_error1, jsonapi:format(Error)}]}
            ), Req1}
    end;

jsonapi_map(Req, List) ->
    ?evman_args([List]),
    {jsonapi:not_extended(wrong_format), Req}.

