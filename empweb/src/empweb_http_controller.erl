

%%
%% Эта форма модуля обработки запроса устарела
%%

-module(empweb_http_controller).
-export([behaviour_info/1]).

%% @private
-spec behaviour_info(_)
    -> undefined | [{handle, 2} | {init, 3} | {terminate, 2}, ...].
behaviour_info(callbacks) ->
    [{init, 3}, {terminate, 2}];
behaviour_info(_Other) ->
    undefined.
