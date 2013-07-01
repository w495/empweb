%% Feel free to use, reuse and abuse the code in this file.

-module(empweb_handler_default).
-behaviour(cowboy_http_handler).

-export([
    init/3,
    handle/2,
    terminate/3
]).

-include("empweb.hrl").



init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req0, State) ->
    {Path, Req1} = cowboy_req:path(Req0),
    {ok, Req2}  = notfound(Path, Req1),
    {ok, Req2, State}.

notfound(Path, Req) ->
    cowboy_req:reply(404, [], [<<"Not Found ">>, Path], Req).

terminate(_Reason, _Req, _State) ->
    ok.


%init(_Transport, Req, []) ->
    %%{ok, Req, undefined}.

%%%
%%% @doc Возвращает 404, вместе с путем, если его размер менее 64.
%%%
%handle(Req, State) ->
     %{Method, Req2} = cowboy_req:method(Req),

    %{ok, Resreq} =
        %cowboy_req:reply(
            %404,
            %?OUTPUT_TEXT_HEADERS,
            %case
                %begin
                    %{Method, erlang:size(Method) > 64 }
                %end
            %of
                %{_, true} ->
                    %<<"Not found">>;
                %{Path, _} ->
                    %[<<"Not found: ">>, Path]
            %end,
            %Req2
        %),
    %{ok, Resreq, State}.

%terminate(_Reason, _Req, _State) ->
    %ok.
