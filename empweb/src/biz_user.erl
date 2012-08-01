%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(biz_user).

%% ---------------------------------------------------------------------------
%% Заголовочные файлы
%% ---------------------------------------------------------------------------

-include("empweb.hrl").
-include("biz_session.hrl").


%% ---------------------------------------------------------------------------
%% Экспортируемые функции
%% ---------------------------------------------------------------------------

-export([
    register/1,
    update/1,
    login/1,
    is_auth/1,
    logout/1,
    get/1,
    get_friends/1,
    add_friend/1,
    delete_friend/1
]).

%% ---------------------------------------------------------------------------
%% Внешние функции
%% ---------------------------------------------------------------------------


is_auth({session_id, Session_id})->
    case biz_session:get(Session_id) of
        [] ->
            false;
        [_H=#biz_session{perm_names=PList}|_T] ->
            true
    end.

register(Params)->
    domain_user:register([
        {phash, phash(proplists:get_value(pass, Params))}
        |Params
    ]).

update(Params)->
    case proplists:get_value(pass, Params) of
        undefined ->
            domain_user:update(Params);
        Pass ->
            domain_user:update([
                {phash, phash(Pass)}
                |Params
            ])
    end.

logout(Params)->
    case domain_user:logout(Params) of
        {ok, Res} ->
            case proplists:get_value(session_id, Params) of
                undefined ->    ok;
                Session_id ->   biz_session:remove(Session_id)
            end,
            {ok, Res};
        {error, Error} ->
            {error, Error}
    end.


get(Params) ->
    domain_user:get(
        Params,
        [
            id,
            nick,
            name,
            email,
            phone,
            fname,
            sname,
            'extract(epoch from birthday) as birthday',
            male,
            city,
            married_status,
            married_id,
            description,
            money,
            status_id,
            authority_id,
            country_id,
            emotion_id,
            mother_id,
            father_id,
            community_id,
            employment,
            hobby,
            allow_auction_offer
        ]
    ).


get_friends(Params)->
    domain_user:get_friends(Params).

add_friend(Params)->
    domain_user:add_friend(Params).

delete_friend(Params)->
    domain_user:delete_friend(Params).

login(Params) ->
    ?debug("login(Params) -> Params = ~p~n", [Params]),
    Nick = proplists:get_value(nick, Params),
    Pass = proplists:get_value(pass, Params),
    Phash = phash(Pass),
    Max_auth_error = 10,
    EC = 0,
    case domain_user:get_opt({nick, Nick}, [id, nick, phash], [perm_names])  of
        {ok, [{Userpl}]} ->
            Perm_names = proplists:get_value(perm_names, Userpl),
            P = proplists:get_value(phash, Userpl),
            ?debug("login(Params) -> P  = ~p~n", [P ]),
            ?debug("login(Params) -> Phash  = ~p~n", [Phash]),
            if
%                 EC >= Max_auth_error ->
%                     throw({auth_count_overflow, Max_auth_error});
                P =/= Phash ->
                    if
                        Max_auth_error - (EC + 1) > 0 ->
                            throw({bad_password, integer_to_list(Max_auth_error - (EC + 1))});
                        true ->
                            throw({auth_count_overflow, Max_auth_error})
                    end;
                true  ->
                    if
                        %??? EC > 0 -> csrv:set_auth_count(Nick, 0);
                        true -> ok
                    end,
                    Id = proplists:get_value(id, Userpl),
                    Session_id = biz_session:new(#biz_session{
                        id=Id,
                        nick=Nick,
                        perm_names=Perm_names,
                        phash=Phash
                    }),
                    {ok, [{User}]} = domain_user:login(Params),
                    {ok, [{[{session_id, Session_id}|User]}]}

            end;
            _ ->
                throw(bad_user)
    end.

%% ---------------------------------------------------------------------------
%% Внутрениие функции
%% ---------------------------------------------------------------------------

phash(Pass) ->
    hexstring(erlang:md5(Pass)).

hexstring(<<X:128/big-unsigned-integer>>) ->
    erlang:list_to_binary(io_lib:format("~32.16.0B", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    erlang:list_to_binary(io_lib:format("~40.16.0B", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    erlang:list_to_binary(io_lib:format("~64.16.0B", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    erlang:list_to_binary(io_lib:format("~128.16.0B", [X])).

