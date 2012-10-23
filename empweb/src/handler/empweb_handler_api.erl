%% Feel free to use, reuse and abuse the code in this file.

-module(empweb_handler_api).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-include("empweb.hrl").

-record(hstate,{
    path,
    module
 }).

init({_Any, http}, Req, [] = _Opts) ->
    {ok, Req, #hstate{}}.

handle(_Req, #hstate{module=_Module}) ->
    
    {ok, [], #hstate{}}.

terminate(_Req, _State) ->
    ok.
