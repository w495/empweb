%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_pers
-module(biz_pers).

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
    get_perm_names/1,
    has_perm/2,
    has_perm/3,
    logout/1,
    get_pers_id/1,
    get_pers_nick/1,
    get/1,
    get_friends/1,
    add_friend/1,
    delete_friend/1
]).


%%
%% Exported Functions
%%
-export([
    get_pgroup/1,
    update_pgroup/1
]).

%%
%% Exported Functions
%%
-export([
    get_pstatus/1,
    update_pstatus/1
]).

%%
%% Exported Functions
%%
-export([
    get_mstatus/1,
    update_mstatus/1
]).

%%
%% Exported Functions
%%
-export([
    get_authority/1,
    update_authority/1
]).


%%
%% Exported Functions
%%
-export([
    get_emotion/1,
    update_emotion/1
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

get_perm_names({session_id, Session_id})->
    case biz_session:get(Session_id) of
        [] ->
            [];
        [_H=#biz_session{perm_names=Plist}|_] ->
            lists:map(fun convert:to_atom/1, Plist)
    end;

get_perm_names(Session_id)->
    get_perm_names({session_id, Session_id}).


has_perm({session_id, Session_id}, Perm, Flag) ->
    Flag or lists:member(Perm, get_perm_names({session_id, Session_id}));

has_perm(Session_id, Perm, Flag)->
    has_perm(Session_id, Perm, Flag).

has_perm({session_id, Session_id}, Perm)->
    has_perm({session_id, Session_id}, Perm, false);

has_perm(Session_id, Perm)->
    has_perm(Session_id, Perm).


get_pers_id({session_id, Session_id})->
    case biz_session:get(Session_id) of
        [] ->
            undefined;
        [_H=#biz_session{id=Id}|_] ->
            Id
    end.

get_pers_nick({session_id, Session_id})->
    case biz_session:get(Session_id) of
        [] ->
            undefined;
        [_H=#biz_session{id=Id}|_] ->
            Id
    end.

%% ---------------------------------------------------------------------------

register(Params)->
    ?evman_args(Params, <<"pers try to register">>),
    domain_pers:register([
        {phash, phash(proplists:get_value(pass, Params))}
        |Params
    ]).

update(Params)->
    ?evman_args(Params, <<"pers try to update himself">>),
    case proplists:get_value(pass, Params) of
        undefined ->
            domain_pers:update(Params);
        Mbpass ->
            domain_pers:update([
                {phash, phash(Mbpass)}
                |Params
            ])
    end.


get([]) ->
    ?evman_args([[]], <<"get all perss">>),
    domain_pers:get([],[]);

get(Params) ->
    ?evman_args(Params, <<"get pers">>),
    domain_pers:get(Params, []).


get_friends(Params)->
    domain_pers:get_friends(Params).

add_friend(Params)->
    domain_pers:add_friend(Params).

delete_friend(Params)->
    domain_pers:delete_friend(Params).


%%
%% Вход пользователя. Создание сессии.
%%
login(Params) ->
    ?evman_args(Params, <<"pers try to login">>),

    case domain_pers:login(Params) of
        {ok, [{Userpl}]} ->
            Id          = proplists:get_value(id,           Userpl),
            Login       = proplists:get_value(login,        Userpl),
            Perm_names  = proplists:get_value(perm_names,   Userpl),
            Session_id  = biz_session:new(#biz_session{
                id          =   Id,
                login       =   Login,
                perm_names  =   Perm_names
            }),
            {ok, [{User}]} = domain_pers:get(Params),
            ?evman_info({login, [
                {pers,          User},
                {session_id,    Session_id}
            ]}),
            {ok, [{[{session_id, Session_id}|User]}]};
        {error, Error} ->
            {error, Error}
    end.



%%
%% Выход пользователя, Удаление сессии.
%%
logout(Params)->
    ?evman_args(Params, <<"pers try to logout">>),
    Id = proplists:get_value(id, Params),
    case domain_pers:logout(Params) of
        {ok, [{Userpl}]} ->
            case proplists:get_value(session_id, Params) of
                undefined ->
                    {error,{bad_session,{[{id, Id}]}}};
                Session_id ->
                    case biz_session:get({uid, Session_id}) of
                        [#biz_session{id=Id}] ->
                            biz_session:remove(Session_id),
                            {ok, [{[{session_id, Session_id}|Userpl]}]};
                        _ ->
                            {error,{bad_session,{[{id, Id}]}}}
                    end
            end;
        {error, Error} ->
            {error, Error};
        _ ->
            {error,{bad_pers,{[{login, Id}]}}}
    end.



get_emotion(Params) ->
    ?evman_args(Params, <<"get emotion">>),
    domain_pers:get_emotion(Params).

update_emotion(Params) ->
    ?evman_args(Params, <<"get update_emotion">>),
    domain_pers:update_emotion(Params).

get_pgroup(Params) ->
    ?evman_args(Params, <<"get pgroup">>),
    domain_pers:get_pgroup(Params).

update_pgroup(Params) ->
    ?evman_args(Params, <<"update pgroup">>),
    domain_pers:update_pgroup(Params).


get_mstatus(Params) ->
    ?evman_args(Params, <<"get mstatus">>),
    domain_pers:get_mstatus(Params).

update_mstatus(Params) ->
    ?evman_args(Params, <<"update mstatus">>),
    domain_pers:update_mstatus(Params).


get_pstatus(Params) ->
    ?evman_args(Params, <<"get pstatus">>),
    domain_pers:get_pstatus(Params).

update_pstatus(Params) ->
    ?evman_args(Params, <<"update pstatus">>),
    domain_pers:update_pstatus(Params).


get_authority(Params) ->
    ?evman_args(Params, <<"get authority">>),
    domain_pers:get_authority(Params).

update_authority(Params) ->
    ?evman_args(Params, <<"update authority">>),
    domain_pers:update_authority(Params).


%% ---------------------------------------------------------------------------
%% Внутрениие функции
%% ---------------------------------------------------------------------------

phash(Mbpass) ->
    hexstring(erlang:md5(Mbpass)).

hexstring(<<X:128/big-unsigned-integer>>) ->
    erlang:list_to_binary(io_lib:format("~32.16.0B", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    erlang:list_to_binary(io_lib:format("~40.16.0B", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    erlang:list_to_binary(io_lib:format("~64.16.0B", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    erlang:list_to_binary(io_lib:format("~128.16.0B", [X])).

