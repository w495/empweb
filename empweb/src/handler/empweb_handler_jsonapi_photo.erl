%% Feel free to use, reuse and abuse the code in this file.

-module(empweb_handler_jsonapi_photo).
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
    empweb_hap,
    files = []
}).

-record(partstate, {
    fileinfo,
    fileextension,
    nchanks,
    contenttype,
    contentdisposition,
    filename,
    headers,
    body
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
    Reply =
        empweb_http:reply(
            Http_resp#http_resp{body = Http_resp_json},
            Reqres
        ),
    ?evman_debug({reply, Reply}, <<"server reply">>),
    {ok,Reply,State}.

terminate(_Req, _State) ->
    ok.

handle_post(Req, State) ->
    case catch empweb_http:multipart_data(Req) of
        {'EXIT', _} ->
            {empweb_jsonapi:not_extended(no_files), Req};
        {Pbody, Req1} ->
            handle_body(Req1, Pbody, State)
    end.

handle_body(Req, Pbody, State) ->
    case acc_part(Req, [], Pbody, State) of
        {[#partstate{fileinfo=Empweb_resp}], Req1} ->
            {Empweb_resp, Req1};
        {Partstates, Req1} ->
            {
                empweb_jsonapi:fname(empweb_jsonapi:resp(
                    {ok,
                        lists:foldl(
                            fun(#partstate{fileinfo=#empweb_resp{body = {Bpl}}}, Acc)->
                                [{proplists:delete(fname, Bpl)}|Acc]
                            end,
                            [],
                            Partstates
                        )
                    }
                ), <<"upload_file">>),
                Req1
            }
    end.

handle_part(Req, Acc, State) ->
    {Result, Req2} = empweb_http:multipart_data(Req),
    acc_part(Req2, Acc, Result, State).

acc_part(Req, Acc, {headers, Headers}, State) ->
    Contentdisposition =   proplists:get_value(<<"Content-Disposition">>, Headers),
    Contenttype   =   proplists:get_value('Content-Type', Headers),
    Filename =
        case re:run(Contentdisposition, "filename=\"(.+)\"", []) of
            {match,[_,Poslen]} ->
                binary:part(Contentdisposition, Poslen);
            _ ->
                <<"undefined">>
        end,
    Fileextension =
        case re:run(Filename, ".+[.](.+)$", []) of
            {match,[_,Poslen1]} ->
                binary:part(Filename, Poslen1);
            _ ->
                <<"undefined">>
        end,
    Partstate = #partstate{
        headers=Headers,
        nchanks=0,
        body=[],
        contentdisposition = Contentdisposition,
        contenttype=Contenttype,
        filename=Filename,
        fileextension = Fileextension
    },

    handle_part(Req, [Partstate|Acc], State);

acc_part(
    Req,
    [   #partstate{
            body    =   Body,
            nchanks =   Nchanks
        } = Partstate
        |Acc
    ],
    {body, Data},
    State
) ->
    %% Здесь можно сразу писать в файл.
    %% Дописывать Data в его конец.
    %% Но это может оказаться не очень эффективно.
    handle_part(
        Req,
        [   Partstate#partstate{
                body    =   [Data|Body],
                nchanks =   Nchanks + 1
            }
            |Acc
        ],
        State
    );

acc_part(
    Req,
    [   #partstate{
            body        =   Body,
            nchanks     =   Nchanks,
            filename    =   Filename,
            fileextension = Fileextension,
            contentdisposition = Contentdisposition,
            contenttype = Contenttype
        } = Partstate
        |Acc
    ],
    end_of_part,
    State
) ->
    Fullbody = list_to_binary(lists:reverse(Body)),
    Hap =  State#state.empweb_hap#empweb_hap{
        handler         = empweb_jsonapi_file,
        action          = create,
        params          = [
            {isres,         false},
            {filebody,      Fullbody},
            {filename,      Filename},
            {nchanks,       Nchanks},
            {contenttype,   Contenttype},
            {fileextension, Fileextension}
        ]
    },
    {Fileinfo, Req1} = empweb_jsonapi:call(Req, Hap, <<"upload_file">>),
    io:format("Fileinfo = ~p~n", [Fileinfo]),
    handle_part(
        Req1,
        [   Partstate#partstate{
                fileinfo    =   Fileinfo,
                body        =   [],
                nchanks     =   Nchanks + 1
            }
            |Acc
        ],
        State
    );

acc_part(Req, Acc, eof, State) ->
    {lists:reverse(Acc), Req}.

