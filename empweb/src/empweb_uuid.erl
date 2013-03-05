-module(empweb_uuid).
-export([
    v4/0,
    to_string/1,
    to_stringm/1,
    to_stringr/1,
    get_parts/1
]).

-import(random).

v4() ->
    v4(random:uniform(round(math:pow(2, 48))) - 1, random:uniform(round(math:pow(2, 12))) - 1, random:uniform(round(math:pow(2, 32))) - 1, random:uniform(round(math:pow(2, 30))) - 1).
v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

to_string(U) ->
    erlang:list_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).


to_stringm(U) ->
    erlang:list_to_binary(io_lib:format("~.36.0b-~.36.0b-~.36.0b-~.36.0b~.36.0b-~.36.0b", get_parts(U))).


to_stringr(U) ->
    erlang:list_to_binary(io_lib:format(
    "~."++
    empweb_convert:to_list(crypto:rand_uniform(16,36)) ++
    ".0b~."++
    empweb_convert:to_list(crypto:rand_uniform(16,36)) ++
    ".0b~."++
    empweb_convert:to_list(crypto:rand_uniform(16,36)) ++
    ".0b~." ++
    empweb_convert:to_list(crypto:rand_uniform(16,36)) ++
    ".0b~." ++
    empweb_convert:to_list(crypto:rand_uniform(16,36)) ++
    ".0b~." ++
    empweb_convert:to_list(crypto:rand_uniform(16,36)) ++
    ".0b", get_parts(U))).

get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].
