-ifndef(__EMPWEB_474797140__).
-define(__EMPWEB_474797140__, true).


%%
%%
%%
-record(empweb_ctl, {
    ctl,
    act,
    opt = []
}).


%%
%%
%%
-record(empweb_hap, {
    handler,
    action,
    params = [],
    is_auth = true
}).


%%
%% @doc Временный контейнер для ответа сервера.
%%      При прочих равных условиях лучше использовать его.
%%
-record(empweb_resp, {
    status = 200,
    format  = json,
    cookies = [],
    headers = [],
    body = []
}).


%%
%%
%%
-record(http_resp, {
    status = 200,
    headers = [],
    body = []
}).



%%
%% =========================================================================
%% =========================================================================
%%

%%
%% =========================================================================
%% =========================================================================
%%

-define(debug(S),       io:format("debug ~p (~p):" ++ S ++ "~n", [calendar:local_time(), ?MODULE])).
-define(debug(S, P),    io:format("debug ~p (~p):" ++ S ++ "~n", [calendar:local_time(), ?MODULE|P])).

-define(d(S),       io:format("debug ~p (~p):" ++ S ++ "~n", [calendar:local_time(), ?MODULE])).
-define(d(S, P),    io:format("debug ~p (~p):" ++ S ++ "~n", [calendar:local_time(), ?MODULE|P])).




%%
%% ========================================================================= 
%% =========================================================================
%%

%%
%% @doc Размер пула Cowboy по-умолчанию.
%%
-define(EMPWEB_NBACCEPTORS_DEF, 16).

%%
%% @doc Порт по-умолчанию для HTTP.
%%
-define(EMPWEB_CPORT_HTTP_DEF,  8080).

%%
%% @doc Порт по-умолчанию для HTTPS.
%%
-define(EMPWEB_CPORT_HTTPS_DEF, 8443).

%%
%% =========================================================================
%% ========================================================================= 
%%



%%
%% ========================================================================= 
%%

%%
%% @doc  Заголовок Content-Type для JSON 
%%
-define(OUTPUT_JSON_HEADER_CTYPE,
    {'Content-Type', <<"application/json;charset=UTF-8">>}
).

%%
%% @doc Список заголовков для JSON 
%%
-define(OUTPUT_JSON_HEADERS,[
    ?OUTPUT_JSON_HEADER_CTYPE
]).

%%
%% ========================================================================= 
%%

%%
%% @doc  Заголовок Content-Type для XML 
%%
-define(OUTPUT_XML_HEADER_CTYPE,
    {'Content-Type', <<"application/xml;charset=UTF-8">>}
).

%%
%% @doc Список заголовков для XML 
%%
-define(OUTPUT_XML_HEADERS,[
    ?OUTPUT_XML_HEADER_CTYPE
]).

%%
%% ========================================================================= 
%%

%%
%% @doc Заголовок Content-Type для HTML 
%%
-define(OUTPUT_HTML_HEADER_CTYPE,
    {'Content-Type', <<"text/html;charset=UTF-8">>}
).

%%
%% @doc Список заголовков для HTML 
%%
-define(OUTPUT_HTML_HEADERS,[
    ?OUTPUT_HTML_HEADER_CTYPE
]).

%%
%% ========================================================================= 
%%

%%
%% @doc Заголовок Content-Type для TXT 
%%
-define(OUTPUT_TEXT_HEADER_CTYPE,
    {'Content-Type', <<"text/plain;charset=UTF-8">>}
).

%%
%% @doc Список заголовков для TXT 
%%
-define(OUTPUT_TEXT_HEADERS,[
    ?OUTPUT_TEXT_HEADER_CTYPE
]).

%%
%% ========================================================================= 
%%

-endif. %%% __EMPWEB_474797140__


