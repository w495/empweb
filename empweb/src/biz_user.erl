%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(biz_user).

%% ---------------------------------------------------------------------------
%% Заголовочные файлы
%% ---------------------------------------------------------------------------

-include("empweb.hrl").
-include("biz_session.hrl").

%%
%% Описание записей событий и макросов
%%
-include_lib("evman/include/events.hrl").


%%
%% Трансформация для получения имени функции.
%%
-include_lib("evman/include/evman_transform.hrl").


%%


%% ---------------------------------------------------------------------------
%% Экспортируемые функции
%% ---------------------------------------------------------------------------

-export([
    register/1,
    update/1,
    login/1,
    is_auth/1,
    perms/1,
    has_perm/2,
    has_perm/3,
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
        [_H=#biz_session{perm_names=Plist}|_] ->
            true
    end;

is_auth(Session_id)->
    is_auth({session_id, Session_id}).

perms({session_id, Session_id})->
    case biz_session:get(Session_id) of
        [] ->
            [];
        [_H=#biz_session{perm_names=Plist}|_] ->
            lists:map(fun convert:to_atom/1, Plist)
    end;

perms(Session_id)->
    perms({session_id, Session_id}).


has_perm({session_id, Session_id}, Perm, Flag) ->
    Flag or lists:member(Perm, perms({session_id, Session_id}));

has_perm(Session_id, Perm, Flag)->
    has_perm(Session_id, Perm, Flag).

has_perm({session_id, Session_id}, Perm)->
    has_perm({session_id, Session_id}, Perm, false);

has_perm(Session_id, Perm)->
    has_perm(Session_id, Perm).


%% ---------------------------------------------------------------------------

register(Params)->
    ?evman_args(Params, <<"user try to register">>),
    
    domain_user:register([
        {phash, phash(proplists:get_value(pass, Params))}
        |Params
    ]).

update(Params)->
    ?evman_args(Params, <<"user try to update himself">>),
    
    case proplists:get_value(pass, Params) of
        undefined ->
            domain_user:update(Params);
        Pass ->
            domain_user:update([
                {phash, phash(Pass)}
                |Params
            ])
    end.


get(all) ->
    ?evman_args([all], <<"get all users">>),
    domain_user:get(
        all,
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
    );

get(Params) ->
    ?evman_args(Params, <<"get user">>),
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


%%
%% Вход пользователя. Создание сессии.
%%
login(Params) ->
    ?evman_args(Params, <<"user try to login">>),

    Nick = proplists:get_value(nick, Params),
    Pass = proplists:get_value(pass, Params),
    Phash = phash(Pass),
    Max_auth_error = 10,
    EC = 0,
    
    case domain_user:get_opt({nick, Nick}, [id, nick, phash], [perm_names])  of
        {ok, [{Userpl}]} ->
            Perm_names = proplists:get_value(perm_names, Userpl),
            P = proplists:get_value(phash, Userpl),
            if
%                 EC >= Max_auth_error ->
%                     throw({auth_count_overflow, Max_auth_error});
                P =/= Phash ->
                    if
                        Max_auth_error - (EC + 1) > 0 ->
                            %throw({bad_password, integer_to_list(Max_auth_error - (EC + 1))});
                            {error,
                                {bad_password,
                                    {[
                                        {max,  Max_auth_error - (EC + 1)},
                                        {nick, Nick},
                                        {pass, Pass}
                                    ]}
                                }
                            };
                        true ->
                            {error,
                                {auth_count_overflow,
                                    {[
                                        {max,  Max_auth_error - (EC + 1)},
                                        {nick, Nick},
                                        {pass, Pass}
                                    ]}
                                }
                            }
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
                    ?evman_info({login, [
                        {user,          User},
                        {session_id,    Session_id}
                    ]}),
                    {ok, [{[{session_id, Session_id}|User]}]}

            end;
            _ ->
                {error,
                    {bad_user,
                        {[
                            {nick, Nick},
                            {pass, Pass}
                        ]}
                    }
                }
    end.

%%
%% Выход пользователя, Удаление сессии.
%%
logout(Params)->
    ?evman_args(Params, <<"user try to logout">>),
    Nick = proplists:get_value(nick, Params),
    case domain_user:logout(Params) of
        {ok, [{Userpl}]} ->
            case proplists:get_value(session_id, Params) of
                undefined ->
                    {error,{bad_session,{[{nick, Nick}]}}};
                Session_id ->
                    case biz_session:get({uid, Session_id}) of
                        [#biz_session{nick=Nick}] ->
                            biz_session:remove(Session_id),
                            {ok, [{[{session_id, Session_id}|Userpl]}]};
                        _ ->
                            {error,{bad_session,{[{nick, Nick}]}}}
                    end
            end;
        {error, Error} ->
            {error, Error};
        _ ->
            {error,{bad_user,{[{nick, Nick}]}}}
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

