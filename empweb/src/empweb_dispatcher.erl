%% Feel free to use, reuse and abuse the code in this file.

-module(empweb_dispatcher).
-export([dispatch/0]).

dispatch()->
    {ok, App} = application:get_application(),
    Options = [{app, App}],

    [
        {'_', [
            {   [<<"jsonapi">>, <<"test">>],
                empweb_handler_jsonapi_test,
                []
            },
            {   [<<"jsonapi">>],
                empweb_handler_jsonapi,
                []
            },

            {   [<<"my_loop_handler">>],
                empweb_my_loop_handler,
                []
            },

            %%
            %% Контроллер отображения логов
            %%
            {[<<".debug">>, <<".logs">>, <<".ws">>],
                empweb_handler_debug_logs_ws, Options},

            %%
            %% Контроллер отображения автоматических тестов
            %%
            {[<<".debug">>, <<".tests">>, '...'],
                empweb_handler_debug_tests, Options},

            {[<<"static">>, '...'],
                empweb_handler_static, [{path, <<"priv/static">>}]},
            
            {[<<"logs">>, '...'],
                empweb_handler_static, [{path, <<"priv/logs">>}]},
            {'_', empweb_handler_default, []}
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
