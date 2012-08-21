-module(jsonapi).

-include("empweb.hrl").

-include_lib("norm/include/norm.hrl").

%%
%% Описание записей событий и макросов
%%
-include_lib("evman/include/events.hrl").


%%
%% Трансформация для получения имени функции.
%%
-include_lib("evman/include/evman_transform.hrl").




-export([
    ok/0,
    ok/1,
    ok/2,
    error/0,
    error/1,
    error/2,
    resp/1,
    resp/2,
    handle_params/2,
    handle_params/3,
    format/1,
    format/2,
    bad_request/0,
    bad_request/1,
    unauthorized/0,
    unauthorized/1,
    payment_required/0,
    payment_required/1,
    forbidden/0,
    forbidden/1,
    not_found/0,
    not_found/1,
    method_not_allowed/0,
    method_not_allowed/1,
    not_acceptable/0,
    not_acceptable/1,
    proxy_authentication_required/0,
    proxy_authentication_required/1,
    request_timeout/0,
    request_timeout/1,
    conflict/0,
    conflict/1,
    gone/0,
    gone/1,
    length_required/0,
    length_required/1,
    precondition_failed/0,
    precondition_failed/1,
    request_entity_too_large/0,
    request_entity_too_large/1,
    request_uri_too_long/0,
    request_uri_too_long/1,
    unsupported_media_type/0,
    unsupported_media_type/1,
    requested_range_not_satisfiable/0,
    requested_range_not_satisfiable/1,
    expectation_failed/0,
    expectation_failed/1,
    im_a_teapot/0,
    im_a_teapot/1,
    unprocessable_entity/0,
    unprocessable_entity/1,
    locked/0,
    locked/1,
    failed_dependency/0,
    failed_dependency/1,
    unordered_collection/0,
    unordered_collection/1,
    upgrade_required/0,
    upgrade_required/1,
    precondition_required/0,
    precondition_required/1,
    too_many_requests/0,
    too_many_requests/1,
    request_header_fields_too_large/0,
    request_header_fields_too_large/1,
    internal_server_error/0,
    internal_server_error/1,
    not_implemented/0,
    not_implemented/1,
    bad_gateway/0,
    bad_gateway/1,
    service_unavailable/0,
    service_unavailable/1,
    gateway_timeout/0,
    gateway_timeout/1,
    http_version_not_supported/0,
    http_version_not_supported/1,
    variant_also_negotiates/0,
    variant_also_negotiates/1,
    insufficient_storage/0,
    insufficient_storage/1,
    not_extended/0,
    not_extended/1,
    network_authentication_required/0,
    network_authentication_required/1,
    unknown_error/1,
    unknown_error/2
]).



bad_request()->
    bad_request(bad_request).

bad_request(Error)->
    ?MODULE:error(bad_request, Error).

unauthorized()->
    unauthorized(unauthorized).

unauthorized(Error)->
    ?MODULE:error(unauthorized, Error).

payment_required()->
    payment_required(payment_required).

payment_required(Error)->
    ?MODULE:error(payment_required, Error).

forbidden()->
    forbidden(forbidden).

forbidden(Error)->
    ?MODULE:error(forbidden, Error).

not_found()->
    not_found(not_found).

not_found(Error)->
    ?MODULE:error(not_found, Error).

method_not_allowed()->
    method_not_allowed(method_not_allowed).

method_not_allowed(Error)->
    ?MODULE:error(method_not_allowed, Error).

not_acceptable()->
    not_acceptable(not_acceptable).

not_acceptable(Error)->
    ?MODULE:error(not_acceptable, Error).

proxy_authentication_required()->
    proxy_authentication_required(proxy_authentication_required).

proxy_authentication_required(Error)->
    ?MODULE:error(proxy_authentication_required, Error).

request_timeout()->
    request_timeout(request_timeout).

request_timeout(Error)->
    ?MODULE:error(request_timeout, Error).

conflict()->
    conflict(conflict).

conflict(Error)->
    ?MODULE:error(conflict, Error).

gone()->
    gone(gone).

gone(Error)->
    ?MODULE:error(gone, Error).

length_required()->
    length_required(length_required).

length_required(Error)->
    ?MODULE:error(length_required, Error).

precondition_failed()->
    precondition_failed(precondition_failed).

precondition_failed(Error)->
    ?MODULE:error(precondition_failed, Error).

request_entity_too_large()->
    request_entity_too_large(request_entity_too_large).

request_entity_too_large(Error)->
    ?MODULE:error(request_entity_too_large, Error).

request_uri_too_long()->
    request_uri_too_long(request_uri_too_long).

request_uri_too_long(Error)->
    ?MODULE:error(request_uri_too_long, Error).

unsupported_media_type()->
    unsupported_media_type(unsupported_media_type).

unsupported_media_type(Error)->
    ?MODULE:error(unsupported_media_type, Error).

requested_range_not_satisfiable()->
    requested_range_not_satisfiable(requested_range_not_satisfiable).

requested_range_not_satisfiable(Error)->
    ?MODULE:error(requested_range_not_satisfiable, Error).

expectation_failed()->
    expectation_failed(expectation_failed).

expectation_failed(Error)->
    ?MODULE:error(expectation_failed, Error).

im_a_teapot()->
    im_a_teapot(im_a_teapot).

im_a_teapot(Error)->
    ?MODULE:error(im_a_teapot, Error).

unprocessable_entity()->
    unprocessable_entity(unprocessable_entity).

unprocessable_entity(Error)->
    ?MODULE:error(unprocessable_entity, Error).

locked()->
    locked(locked).

locked(Error)->
    ?MODULE:error(locked, Error).

failed_dependency()->
    failed_dependency(failed_dependency).

failed_dependency(Error)->
    ?MODULE:error(failed_dependency, Error).

unordered_collection()->
    unordered_collection(unordered_collection).

unordered_collection(Error)->
    ?MODULE:error(unordered_collection, Error).

upgrade_required()->
    upgrade_required(upgrade_required).

upgrade_required(Error)->
    ?MODULE:error(upgrade_required, Error).

precondition_required()->
    precondition_required(precondition_required).

precondition_required(Error)->
    ?MODULE:error(precondition_required, Error).

too_many_requests()->
    too_many_requests(too_many_requests).

too_many_requests(Error)->
    ?MODULE:error(too_many_requests, Error).

request_header_fields_too_large()->
    request_header_fields_too_large(request_header_fields_too_large).

request_header_fields_too_large(Error)->
    ?MODULE:error(request_header_fields_too_large, Error).

internal_server_error()->
    internal_server_error(internal_server_error).

internal_server_error(Error)->
    ?MODULE:error(internal_server_error, Error).

not_implemented()->
    not_implemented(not_implemented).

not_implemented(Error)->
    ?MODULE:error(not_implemented, Error).

bad_gateway()->
    bad_gateway(bad_gateway).

bad_gateway(Error)->
    ?MODULE:error(bad_gateway, Error).

service_unavailable()->
    service_unavailable(service_unavailable).

service_unavailable(Error)->
    ?MODULE:error(service_unavailable, Error).

gateway_timeout()->
    gateway_timeout(gateway_timeout).

gateway_timeout(Error)->
    ?MODULE:error(gateway_timeout, Error).

http_version_not_supported()->
    http_version_not_supported(http_version_not_supported).

http_version_not_supported(Error)->
    ?MODULE:error(http_version_not_supported, Error).

variant_also_negotiates()->
    variant_also_negotiates(variant_also_negotiates).

variant_also_negotiates(Error)->
    ?MODULE:error(variant_also_negotiates, Error).

insufficient_storage()->
    insufficient_storage(insufficient_storage).

insufficient_storage(Error)->
    ?MODULE:error(insufficient_storage, Error).

not_extended()->
    not_extended(not_extended).

not_extended(Error)->
    ?MODULE:error(not_extended, Error).

network_authentication_required()->
    network_authentication_required(network_authentication_required).

network_authentication_required(Error)->
    ?MODULE:error(network_authentication_required, Error).


unknown_error(Error)->
        internal_server_error(
            {[
                {unknown_error, format(Error)}
            ]}
        ).

unknown_error(Eclass, Ereason)->
        internal_server_error(
            {[
                {unknown_error,
                    {[
                        {class, format(Eclass)},
                        {reason,format(Ereason)}
                    ]}
                }
            ]}
        ).

%%% -------------------------------------------------------------------------
ok() ->
    ok({ok, ok}).

ok({ok, Body}) ->
    ok(ok, {ok, Body});
ok(Body) ->
    ok(ok, {ok, Body}).

ok(Code, {ok, Body}) ->
    #empweb_resp{
        status  = Code,
        format = json,
        body = {[{ok, Body}]}
    };
ok(Code, Body) ->
    ok(Code, {ok, Body}).

%%% -------------------------------------------------------------------------


%%% -------------------------------------------------------------------------
error() ->
    ?MODULE:error({error, error}).

error({error, Error}) ->
    ?MODULE:error(error, {error, Error});
error(Error) ->
    ?MODULE:error(error, {error, Error}).

error(Code, {error, Error}) ->
    ?evman_error({http,[{code, Code},{error, Error}]}),
    #empweb_resp{
        status  = Code,
        format = json,
        body = {[{error, Error}]}
    };
error(Code, Error) ->
    ?MODULE:error(Code, {error, Error}).

%%% -------------------------------------------------------------------------

format(Trem) ->
    format("~p", [Trem]).

format(Format, List) when erlang:is_list(List) ->
    erlang:list_to_binary(io_lib:format(Format,List));
    
format(Format, Trem) ->
    format(Format, [Trem]).
    
%%% -------------------------------------------------------------------------

resp({error, {not_exists, Object}}) ->
    gone({[{bad_session, Object}]});
    
resp({error, {bad_session, Object}}) ->
    forbidden({[{bad_session, Object}]});

resp({error, {bad_user, Object}}) ->
    forbidden({[{bad_user, Object}]});

resp({error, {bad_password, Object}}) ->
    forbidden({[{bad_password, Object}]});
    
resp({error, {Reason, Object}}) ->
    not_extended({[{Reason, Object}]});
resp({error, Error}) ->
    ?MODULE:error({error, Error});

resp({ok, Body}) ->
    ok({ok, Body}).

resp(Code, {error, Error}) ->
    ?MODULE:error(Code,{error, Error});
resp(Code,{ok, Body}) ->
    ok(Code, {ok, Body}).

%%% -------------------------------------------------------------------------


handle_params(Data, Function) ->
    handle_params(Data, Function, []).


handle_params(Data, Function, Pstate) ->
    ?evman_debug(
        [   {data, Data},
            {function, Function}
        ],
        <<" = data & function">>
    ),

    case Data#norm.errors of
        [] ->
            case erlang:apply(Function, [Data]) of
                {ok, Reply, State} ->
                    ?evman_debug(
                        Reply,
                        <<" = reply">>
                    ),
                    {ok, Reply, State};
                {error, {Reason, Object}} ->
                    ?evman_error(
                        {error, [
                            {reason, Reason},
                            {object, Object}
                        ]},
                        <<" = error">>
                    ),
                    {ok, not_extended({[{Reason, Object}]}), Pstate};
                {error, Error} ->
                    ?evman_error(
                        {error, Error},
                        <<" = error">>
                    ),
                    {ok, ?MODULE:error(), Pstate};
                Some ->
                    ?evman_error(
                        {error, Some},
                        <<" = error">>
                    ),
                    {ok, ?MODULE:error(), Pstate}
            end;
        Errors ->
            ?evman_error(
                {wrong_format, Errors},
                <<" = wrong format">>
            ),
            {ok, not_extended(wrong_format), Pstate}
    end.

