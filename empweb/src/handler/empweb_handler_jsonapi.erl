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


%
% -define(APPLICATION, application:get_application(?MODULE)).
%


init(_, Req, _Opts) ->
    ?evman_warning({erlang:time(), Req}),
    {Auth, Req1}    =   empweb_http:auth(Req),
    Is_auth         =   empweb_biz_pers:is_auth(Auth),
    Pid             =   empweb_biz_pers:get_pers_id(Auth),
    Pnick           =   empweb_biz_pers:get_pers_nick(Auth),
    Pperm_names     =   empweb_biz_pers:get_perm_names(Auth),
    {ok, Req1, #state{
        empweb_hap  =
            #empweb_hap {
                auth            =   Auth,
                is_auth         =   Is_auth,
                pers_id         =   Pid,
                pers_nick       =   Pnick,
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
        %% ----------------------------------------------------------------
            <<"get_firecounts">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_firecounts,
                    action          =   'get'
                };
        %%
        %% Тип разрешения: не рассмотрен, запрещена, разрешена
        %%
            <<"get_oktype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_oktype,
                    action          =   'get'
                };
            <<"get_all_oktypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_oktype,
                    action          =   'get'
                };
            <<"create_oktype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_oktype,
                    action          =   create
                };
            <<"update_oktype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_oktype,
                    action          =   update
                };
            <<"delete_oktype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_oktype,
                    action          =   delete
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

            <<"count_roomlisttype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlisttype,
                    action          =   count
                };
            <<"get_roomlisttype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlisttype,
                    action          =   'get'
                };
            <<"get_all_roomlisttypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlisttype,
                    action          =   'get'
                };
            <<"create_roomlisttype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlisttype,
                    action          =   create
                };
            <<"update_roomlisttype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlisttype,
                    action          =   update
                };
            <<"delete_roomlisttype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlisttype,
                    action          =   delete
                };


            <<"count_roomlist">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlist,
                    action          =   count
                };
            <<"get_roomlist">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlist,
                    action          =   'get'
                };
            <<"get_all_roomlists">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlist,
                    action          =   'get'
                };
            <<"create_roomlist">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlist,
                    action          =   create
                };
            <<"update_roomlist">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlist,
                    action          =   update
                };
            <<"delete_roomlist">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlist,
                    action          =   delete
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

            <<"get_vote">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_vote,
                    action          =   'get'
                };
            <<"get_all_votes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_vote,
                    action          =   'get'
                };
            <<"create_vote">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_vote,
                    action          =   create
                };
            <<"update_vote">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_vote,
                    action          =   update
                };
            <<"delete_vote">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_vote,
                    action          =   delete
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
            <<"get_post_top">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_post_top
                };

            <<"get_all_posts">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   get_post
                };
            <<"repost_post">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   repost_post
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
            <<"repost_comment">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   repost_comment
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
            %% Ccылка (изгнание)
            %%
            <<"get_exile">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_exile,
                    action          =   get
                };
            <<"get_all_exiles">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_exile,
                    action          =   get
                };
            <<"create_exile">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_exile,
                    action          =   create
                };
            <<"update_exile">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_exile,
                    action          =   update
                };
            <<"delete_exile">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_exile,
                    action          =   delete
                };

            %%
            %% Защита от обнуления
            %%
            <<"count_zprotbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_zprotbuy,
                    action          =   count
                };
            <<"get_zprotbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_zprotbuy,
                    action          =   get
                };
            <<"get_all_zprotbuys">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_zprotbuy,
                    action          =   get
                };
            <<"create_zprotbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_zprotbuy,
                    action          =   create
                };
            <<"update_zprotbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_zprotbuy,
                    action          =   update
                };
            <<"delete_zprotbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_zprotbuy,
                    action          =   delete
                };


            %%
            %% Уровни невидимости пользователей
            %%
            <<"count_invistype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invistype,
                    action          =   count
                };
            <<"get_invistype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invistype,
                    action          =   get
                };
            <<"get_all_invistypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invistype,
                    action          =   get
                };
            <<"create_invistype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invistype,
                    action          =   create
                };
            <<"update_invistype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invistype,
                    action          =   update
                };
            <<"delete_invistype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invistype,
                    action          =   delete
                };

            %%
            %% Покупки невидимости
            %%
            <<"count_invisbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invisbuy,
                    action          =   count
                };
            <<"get_invisbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invisbuy,
                    action          =   get
                };
            <<"get_all_invisbuys">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invisbuy,
                    action          =   get
                };
            <<"create_invisbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invisbuy,
                    action          =   create
                };
            <<"update_invisbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invisbuy,
                    action          =   update
                };
            <<"delete_invisbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_invisbuy,
                    action          =   delete
                };

            %%
            %% Чат-комнаты (страны)
            %%
            <<"get_room_photos">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   get_photos
                };
            <<"get_room_posts">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   get_posts
                };
            <<"get_room_blogs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   get_blogs
                };
            <<"count_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   count
                };
            <<"get_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   get
                };
            <<"get_all_rooms">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   get
                };
            <<"join_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   join
                };
            <<"create_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   create
                };
            <<"update_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   update
                };
            <<"delete_room">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   delete
                };
            <<"add_room_topic">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   add_topic
                };
            <<"delete_room_topic">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_room,
                    action          =   delete_topic
                };
            %%
            %% Сообщества
            %%
            <<"get_community_blogs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_community,
                    action          =   get_blogs
                };
            <<"get_community_posts">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_community,
                    action          =   get_posts
                };
            <<"get_community_photos">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_community,
                    action          =   get_photos
                };
            <<"count_community">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_community,
                    action          =   count
                };
            <<"get_community">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_community,
                    action          =   get
                };
            <<"get_all_communities">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_community,
                    action          =   get
                };
            <<"create_community">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_community,
                    action          =   create
                };
            <<"update_community">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_community,
                    action          =   update
                };
            <<"delete_community">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_community,
                    action          =   delete
                };
            <<"add_community_topic">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_community,
                    action          =   add_topic
                };
            <<"delete_community_topic">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_community,
                    action          =   delete_topic
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
            <<"readall_message_for_me">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   readall_message_for_me
                };
            <<"count_message_for_me">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   count_message_for_me
                };
            <<"count_message_from_me">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_doc,
                    action          =   count_message_from_me
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
            <<"get_photo_top">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_photo,
                    action          =   'get_top'
                };
            <<"get_all_photos">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_photo,
                    action          =   'get'
                };
            <<"repost_photo">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_photo,
                    action          =   repost
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
            %%
            %% флаги
            %%
            <<"count_flag">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_flag,
                    action          =   'count'
                };
            <<"get_flag">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_flag,
                    action          =   'get'
                };
            <<"get_all_flags">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_flag,
                    action          =   'get'
                };
            <<"repost_flag">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_flag,
                    action          =   repost
                };
            <<"create_flag">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_flag,
                    action          =   'create'
                };
            <<"update_flag">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_flag,
                    action          =   'update'
                };
            <<"delete_flag">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_flag,
                    action          =   delete
                };
            %%
            %% Фоны
            %%
            <<"count_back">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_back,
                    action          =   'count'
                };
            <<"get_back">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_back,
                    action          =   'get'
                };
            <<"get_all_backs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_back,
                    action          =   'get'
                };
            <<"repost_back">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_back,
                    action          =   repost
                };
            <<"create_back">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_back,
                    action          =   'create'
                };
            <<"update_back">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_back,
                    action          =   'update'
                };
            <<"delete_back">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_back,
                    action          =   delete
                };
            %%
            %% Обои
            %%
            <<"count_wall">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_wall,
                    action          =   'count'
                };
            <<"get_wall">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_wall,
                    action          =   'get'
                };
            <<"get_all_walls">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_wall,
                    action          =   'get'
                };
            <<"repost_wall">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_wall,
                    action          =   repost
                };
            <<"create_wall">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_wall,
                    action          =   'create'
                };
            <<"update_wall">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_wall,
                    action          =   'update'
                };
            <<"delete_wall">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_wall,
                    action          =   delete
                };
            %%
            %% гербы
            %%
            <<"count_arms">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_arms,
                    action          =   'count'
                };
            <<"get_arms">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_arms,
                    action          =   'get'
                };
            <<"get_all_armss">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_arms,
                    action          =   'get'
                };
            <<"repost_arms">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_arms,
                    action          =   repost
                };
            <<"create_arms">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_arms,
                    action          =   'create'
                };
            <<"update_arms">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_arms,
                    action          =   'update'
                };
            <<"delete_arms">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_arms,
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

          <<"get_all_ostatuses">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_ostatus'
                };
            <<"get_ostatus">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_pers,
                    action          =   'get_ostatus'
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

            %% ==================================================
            <<"get_friendtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_friendtype,
                    action          =   'get'
                };
            <<"get_all_friendtypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_friendtype,
                    action          =   'get'
                };
            <<"create_friendtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_friendtype,
                    action          =   create
                };
            <<"update_friendtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_friendtype,
                    action          =   update
                };
            <<"delete_friendtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_friendtype,
                    action          =   delete
                };
            %% ==================================================

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

            <<"count_thingbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingbuy,
                    action          =   count
                };
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

            <<"count_thingwish">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingwish,
                    action          =   count
                };
            <<"get_thingwish">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingwish,
                    action          =   'get'
                };
            <<"get_all_thingwishs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingwish,
                    action          =   'get'
                };
            <<"create_thingwish">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingwish,
                    action          =   create
                };
            <<"update_thingwish">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingwish,
                    action          =   update
                };
            <<"delete_thingwish">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_thingwish,
                    action          =   delete
                };


            %% ==================================================
            <<"count_experbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_experbuy,
                    action          =   'get'
                };
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

            <<"count_roomexperbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomexperbuy,
                    action          =   count
                };
            <<"get_roomexperbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomexperbuy,
                    action          =   'get'
                };
            <<"get_all_roomexperbuys">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomexperbuy,
                    action          =   'get'
                };
            <<"create_roomexperbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomexperbuy,
                    action          =   create
                };
            <<"update_roomexperbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomexperbuy,
                    action          =   update
                };
            <<"delete_roomexperbuy">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomexperbuy,
                    action          =   delete
                };

            %% ==================================================
            <<"count_roomlot">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_roomlot,
                    action          =   count
                };
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
            <<"get_actiontype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_actiontype,
                    action          =   'get'
                };
            <<"get_all_actiontypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_actiontype,
                    action          =   'get'
                };
            <<"create_actiontype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_actiontype,
                    action          =   create
                };
            <<"update_actiontype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_actiontype,
                    action          =   update
                };
            <<"delete_actiontype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_actiontype,
                    action          =   delete
                };


            %% ==================================================
            <<"get_action">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_action,
                    action          =   'get'
                };
            <<"get_all_actions">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_action,
                    action          =   'get'
                };
            <<"create_action">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_action,
                    action          =   create
                };
            <<"update_action">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_action,
                    action          =   update
                };
            <<"delete_action">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_action,
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
            <<"get_communitytreas">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitytreas,
                    action          =   'get'
                };
            <<"get_all_communitytreass">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitytreas,
                    action          =   'get'
                };
            <<"create_communitytreas">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitytreas,
                    action          =   create
                };
            <<"update_communitytreas">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitytreas,
                    action          =   update
                };
            <<"delete_communitytreas">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communitytreas,
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
            <<"get_cptrans">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_cptrans,
                    action          =   'get'
                };
            <<"get_all_cptranss">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_cptrans,
                    action          =   'get'
                };
            <<"create_cptrans">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_cptrans,
                    action          =   create
                };
            <<"update_cptrans">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_cptrans,
                    action          =   update
                };
            <<"delete_cptrans">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_cptrans,
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
            <<"get_attachtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_attachtype,
                    action          =   'get'
                };
            <<"get_all_attachtypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_attachtype,
                    action          =   'get'
                };
            <<"create_attachtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_attachtype,
                    action          =   create
                };
            <<"update_attachtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_attachtype,
                    action          =   update
                };
            <<"delete_attachtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_attachtype,
                    action          =   delete
                };

        %% ==================================================
            <<"get_attach">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_attach,
                    action          =   'get'
                };
            <<"get_all_attachs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_attach,
                    action          =   'get'
                };
            <<"create_attach">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_attach,
                    action          =   create
                };
            <<"update_attach">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_attach,
                    action          =   update
                };
            <<"delete_attach">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_attach,
                    action          =   delete
                };


        %% ==================================================
            <<"count_eventtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventtype,
                    action          =   count
                };
            <<"get_eventtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventtype,
                    action          =   'get'
                };
            <<"get_all_eventtypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventtype,
                    action          =   'get'
                };
            <<"create_eventtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventtype,
                    action          =   create
                };
            <<"update_eventtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventtype,
                    action          =   update
                };
            <<"delete_eventtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventtype,
                    action          =   delete
                };


        %% ==================================================
            <<"count_eventobj">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventobj,
                    action          =   count
                };
            <<"get_eventobj">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventobj,
                    action          =   'get'
                };
            <<"get_all_eventobjs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventobj,
                    action          =   'get'
                };
            <<"create_eventobj">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventobj,
                    action          =   create
                };
            <<"update_eventobj">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventobj,
                    action          =   update
                };
            <<"delete_eventobj">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventobj,
                    action          =   delete
                };


        %% ==================================================
            <<"count_eventact">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventact,
                    action          =   count
                };
            <<"get_eventact">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventact,
                    action          =   'get'
                };
            <<"get_all_eventacts">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventact,
                    action          =   'get'
                };
            <<"create_eventact">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventact,
                    action          =   create
                };
            <<"update_eventact">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventact,
                    action          =   update
                };
            <<"delete_eventact">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventact,
                    action          =   delete
                };

        %% ==================================================
            <<"count_eventspc">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventspc,
                    action          =   count
                };
            <<"get_eventspc">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventspc,
                    action          =   'get'
                };
            <<"get_all_eventspcs">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventspc,
                    action          =   'get'
                };
            <<"create_eventspc">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventspc,
                    action          =   create
                };
            <<"update_eventspc">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventspc,
                    action          =   update
                };
            <<"delete_eventspc">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_eventspc,
                    action          =   delete
                };
        %% ==================================================
            <<"count_event">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_event,
                    action          =   count
                };
            <<"get_event">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_event,
                    action          =   'get'
                };
            <<"get_all_events">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_event,
                    action          =   'get'
                };
            <<"create_event">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_event,
                    action          =   create
                };
            <<"update_event">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_event,
                    action          =   update
                };
            <<"delete_event">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_event,
                    action          =   delete
                };

        %% ==================================================
            <<"count_communityhist">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_communityhist,
                    action          =   'count'
                };
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


        %% ==================================================
            <<"get_claim">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_claim,
                    action          =   'get'
                };
            <<"get_all_claims">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_claim,
                    action          =   'get'
                };
            <<"create_claim">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_claim,
                    action          =   create
                };
            <<"update_claim">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_claim,
                    action          =   update
                };
            <<"delete_claim">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_claim,
                    action          =   delete
                };

            <<"get_claimtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_claimtype,
                    action          =   'get'
                };
            <<"get_all_claimtypes">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_claimtype,
                    action          =   'get'
                };
            <<"create_claimtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_claimtype,
                    action          =   create
                };
            <<"update_claimtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_claimtype,
                    action          =   update
                };
            <<"delete_claimtype">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_claimtype,
                    action          =   delete
                };


        %% ==================================================
            <<"count_service">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_service,
                    action          =   'count'
                };
            <<"get_service">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_service,
                    action          =   'get'
                };
            <<"get_all_services">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_service,
                    action          =   'get'
                };
            <<"create_service">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_service,
                    action          =   create
                };
            <<"update_service">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_service,
                    action          =   update
                };
            <<"delete_service">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_service,
                    action          =   delete
                };


        %% ==================================================
            <<"count_perspichead">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspichead,
                    action          =   count
                };
            <<"get_perspichead">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspichead,
                    action          =   'get'
                };
            <<"get_all_perspicheads">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspichead,
                    action          =   'get'
                };
            <<"create_perspichead">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspichead,
                    action          =   create
                };
            <<"update_perspichead">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspichead,
                    action          =   update
                };
            <<"delete_perspichead">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspichead,
                    action          =   delete
                };


        %% ==================================================
            <<"count_perspicbody">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspicbody,
                    action          =   count
                };
            <<"get_perspicbody">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspicbody,
                    action          =   'get'
                };
            <<"get_all_perspicbodys">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspicbody,
                    action          =   'get'
                };
            <<"create_perspicbody">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspicbody,
                    action          =   create
                };
            <<"update_perspicbody">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspicbody,
                    action          =   update
                };
            <<"delete_perspicbody">> ->
                Eh#empweb_hap{
                    handler         =   empweb_jsonapi_perspicbody,
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
    io:format("Res = ~p~n~n~n", [Res]),
    {lists:reverse(Res), Reqres};

empweb_jsonapi_map(Req, List, State) ->
    ?evman_args([List]),
    {empweb_jsonapi:not_extended(wrong_format), Req}.
