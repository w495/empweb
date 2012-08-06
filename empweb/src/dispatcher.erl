%% Feel free to use, reuse and abuse the code in this file.

-module(dispatcher).
-export([dispatch/0]).

dispatch()->
    {ok, App} = application:get_application(),
    Options = [{app, App}],

    [
        {'_', [
            {[<<"jsonapi">>],           handler_jsonapi,    []},

            {[<<".debug">>, <<".logs">>, <<".ws">>],  handler_websocket, Options},
            
            {[<<"static">>, '...'],     handler_static, [{path, <<"priv/static">>}]},
            {[<<"logs">>, '...'],       handler_static, [{path, <<"priv/logs">>}]},
            {'_', handler_default, []}
        ]}
    ].




%% dispatch()->
%%     [
%%         {[<<"static">> , '_'], [
%% 
%%             {['...'], handler_static, [{path, <<"priv/static">>}]},
%%             {'_', handler_default, []}
%%         ]},
%%         
%%         {'_', [
%% %%             {[<<"websocket">>], handler_websocket, []},
%% %%             {[<<"eventsource">>], handler_eventsource, []},
%% %%             {[<<"eventsource">>, <<"live">>], handler_eventsource_emitter, []},
%% 
%%             {[<<"api">>, '...'],    handler_api,    []},
%%             {[<<"static">>, '...'], handler_static, [{path, <<"priv/static">>}]},
%%             {'_', handler_default, []}
%%         ]}
%%     ].
%% 
