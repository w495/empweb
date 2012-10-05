%%
%% @file    biz_pers.erl
%%          Бизнес логика по работе с документами,
%%          и связанными с ними объектами.
%%
%%          Зависит от модулей:
%%              * domain_pers;
%%              * biz_session.
%%          Все внешние функуции принимают
%%              proplist()
%%          и возвращают:
%%              {ok, [Obj::{proplist()}]}
%%              |   {error, Reason::atom()}
%%              |   {error, {Reason::atom(), Info::atom()}}
%%              |   {error, {Reason::atom(), Info::[Obj::{proplist()}]}}
%%
-module(biz_pers).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Определения общие для всего приложения
%%
-include("empweb.hrl").

%%
%% Описание сессии пользователя
%%
-include("biz_session.hrl").

%%
%% Описание записей событий и макросов
%%
-include_lib("evman/include/events.hrl").

%%
%% Трансформация для получения имени функции.
%%
-include_lib("evman/include/evman_transform.hrl").

%% ===========================================================================
%% Экспортируемые функции
%% ===========================================================================

%%
%% Сам пользователь
%%
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
    pass/1
]).

%%
%% Группа пользователя
%%
-export([
    get_pgroup/1,
    create_pgroup/1,
    update_pgroup/1,
    delete_pgroup/1
]).

%%
%% Друзья пользователя
%%
-export([
    get_friends/1,
    add_friend/1,
    delete_friend/1
]).

%%
%% Статус пользователя
%%
-export([
    get_pstatus/1,
    create_pstatus/1,
    update_pstatus/1,
    delete_pstatus/1
]).

%%
%% Семейное положение пользователя
%%
-export([
    get_mstatus/1,
    create_mstatus/1,
    update_mstatus/1,
    delete_mstatus/1
]).

%%
%% Авторитет пользователя
%%
-export([
    get_authority/1,
    create_authority/1,
    update_authority/1,
    delete_authority/1
]).


%%
%% Эмоции пользователя
%%
-export([
    get_emotion/1,
    create_emotion/1,
    update_emotion/1,
    delete_emotion/1
]).

%%
%% Структура данных для отправки сообщений.
%%
-record(send, {
    type            :: phone|email,
        %% Тип сообщения.
    destination     :: integer()|binary(),
        %% Адрес\номер телефона
        %%      integer  для phone
        %%      binary   для mail
    message         :: any()
        %% Некоторая структура, которую хотим передать.
        %% Внешний вид сообщения, определяет передающий элемент.
}).


%% ===========================================================================
%% Внешние функции
%% ===========================================================================

is_auth({session_id, Session_id})->
    case biz_session:get(Session_id) of
        [] ->
            false;
        [_H=#biz_session{perm_names=Plist}|_] when erlang:is_list(Plist) ->
            true;
        _ ->
            []
    end;

is_auth(Session_id)->
    is_auth({session_id, Session_id}).

get_perm_names({session_id, Session_id})->
    case biz_session:get(Session_id) of
        [] ->
            [];
        [_H=#biz_session{perm_names=Plist}|_] when erlang:is_list(Plist)->
            lists:map(fun convert:to_atom/1, Plist);
        _ ->
            []
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
    domain_pers:register(Params).

update(Params)->
    ?evman_args(Params, <<"pers try to update him self">>),
    domain_pers:update(Params).



get(Params) ->
    ?evman_args(Params, <<"get pers">>),
    domain_pers:get(
        Params,
        case proplists:get_value(fields, Params) of
            undefined ->
                dao_pers:table({fields, select}) -- [phash];
            Fields ->
                Fields
        end
    ).

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
            ?evman_info({login, [
                {pers,          Userpl},
                {session_id,    Session_id}
            ]}),
            {ok, [{[{session_id, Session_id}|proplists:delete(perm_names,   Userpl)]}]};
        {error, Error} ->
            {error, Error}
    end.

%%
%% Восстановление пароля, проверки применимости.
%%
pass(Params) ->
    ?evman_args(Params, <<"pers try to remind password">>),
    Id = proplists:get_value(id,Params),
    Pers_id = proplists:get_value(pers_id,Params),
    %%
    %% Востанавливать пароль можно только, если пользователь не залогинен
    %% в системе, или если залогинен, но восстанавливает пароль для себя.
    %%
    case {proplists:get_value(is_auth,Params), Id == Pers_id} of
        {true, true}->
            restore_pass(Params);
        {false, _}->
            restore_pass(Params);
        {undefined, _}->
            restore_pass(Params);
        _ ->
            {error,{bad_pers,{[{id, Id}]}}}
    end.

%%
%%  Восстановление пароля (непосредственно).
%%  Сначала посылаем сообщение о новом пароле,
%%  потом сбрасываем старый пароль.
%%
restore_pass(Params) ->
    Id = proplists:get_value(id,Params),
    Email = proplists:get_value(email,Params),
    case domain_pers:get(Params, [email, phone]) of
        {ok,[]} ->
            {error,{bad_pers,{[{id, Id}, {email,Email}]}}};
        {ok,[{Perspl}]} ->
            case Email == proplists:get_value(email, Perspl) of
                true ->
                    {ok, Pass} = lgps:new(),
                    {Status, Reasons} = lists:foldl(fun(Type, {Status, Reasons})->
                        case restore_pass_send_guarded(#send{
                            type        =   Type,
                            message     =   {pass,  Pass},
                            destination =   proplists:get_value(Type, Perspl)
                        }) of
                            {ok,    was_sent} ->
                                {true or Status, Reasons};
                            {error, Reason} ->
                                {false or Status, [{Type, Reason}|Reasons]}
                        end
                    end, {false, []}, [email, phone]),
                    case Status of
                        true ->
                            case domain_pers:update([{id, Id}, {pass, Pass}]) of
                                {ok, [{Upl}]} ->
                                    {ok, [{[
                                        {errors, [{Reasons}]}
                                        | Upl
                                    ]}]};
                                {error,{required,[username,password]}} ->
                                    {error,{this_is_test_user,{[{id, Id}]}}}
                            end;
                        false ->
                            {error,{no_enough_info,{[{id, Id}]}}}
                    end;
                _ -> 
                    io:format("~n ==> ~p ~n", [Perspl]),
                    {error,{bad_pers,{[{id, Id}, {email,Email}]}}}
            end;
        {error, Error} ->
            {error, Error}
    end.

restore_pass_send_guarded(Send) ->
    %%
    %% TODO: сделать проверку через капчу
    %%
    restore_pass_send(Send).

%%
%% Непосредственная посылка сообщения о новом пароле
%%
restore_pass_send(#send{destination=null}) ->
    {error, no_information};

restore_pass_send(#send{destination=undefined}) ->
    {error, no_information};

restore_pass_send(#send{type=phone, destination=Phone}) ->
    {error, not_implemented};

restore_pass_send(#send{type=email, destination=Email, message={pass,  Pass}}) ->
    %%
    %% TODO: сделать нормальную посылку почты
    %%
    case mail_utils:mail(
        Email,
        [<<"Empire user: ">>, Email],
        <<"Empire notification">>,
        [<<"Your new password is: ">>, Pass]
    ) of
        {ok, _} ->
            {ok,was_sent};
        {error, Error}->
            {error, Error}
    end.

%%
%% Выход пользователя, Удаление сессии.
%%
logout(Params)->
    io:format("Params = ~p~n ", [Params]),
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
            {error,{bad_pers,{[{id, Id}]}}}
    end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Друзья пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_friends(Params)->
    domain_pers:get_friends(Params).

add_friend(Params)->
    domain_pers:add_friend(Params).

delete_friend(Params)->
    domain_pers:delete_friend(Params).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Эмоции пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_emotion(Params) ->
    ?evman_args(Params, <<"get emotion">>),
    domain_pers:get_emotion(Params).

create_emotion(Params) ->
    ?evman_args(Params, <<"get create emotion">>),
    domain_pers:create_emotion(Params).

update_emotion(Params) ->
    ?evman_args(Params, <<"get update emotion">>),
    domain_pers:update_emotion(Params).

delete_emotion(Params) ->
    ?evman_args(Params, <<"get delete emotion">>),
    domain_pers:delete_emotion(Params).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Группа пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_pgroup(Params) ->
    ?evman_args(Params, <<"get pgroup">>),
    domain_pers:get_pgroup(Params).

create_pgroup(Params) ->
    ?evman_args(Params, <<"create pgroup">>),
    domain_pers:create_pgroup(Params).

update_pgroup(Params) ->
    ?evman_args(Params, <<"update pgroup">>),
    domain_pers:update_pgroup(Params).

delete_pgroup(Params) ->
    ?evman_args(Params, <<"update pgroup">>),
    domain_pers:delete_pgroup(Params).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Семейное положение пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_mstatus(Params) ->
    ?evman_args(Params, <<"get mstatus">>),
    domain_pers:get_mstatus(Params).

create_mstatus(Params) ->
    ?evman_args(Params, <<"create mstatus">>),
    domain_pers:create_mstatus(Params).
    
update_mstatus(Params) ->
    ?evman_args(Params, <<"update mstatus">>),
    domain_pers:update_mstatus(Params).

delete_mstatus(Params) ->
    ?evman_args(Params, <<"delete mstatus">>),
    domain_pers:delete_mstatus(Params).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Статус пользователя пользователя: в сети \ не в сети.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_pstatus(Params) ->
    ?evman_args(Params, <<"get pstatus">>),
    domain_pers:get_pstatus(Params).

create_pstatus(Params) ->
    ?evman_args(Params, <<"create pstatus">>),
    domain_pers:create_pstatus(Params).

update_pstatus(Params) ->
    ?evman_args(Params, <<"update pstatus">>),
    domain_pers:update_pstatus(Params).

delete_pstatus(Params) ->
    ?evman_args(Params, <<"delete pstatus">>),
    domain_pers:delete_pstatus(Params).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Авторитет пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_authority(Params) ->
    ?evman_args(Params, <<"get authority">>),
    domain_pers:get_authority(Params).

create_authority(Params) ->
    ?evman_args(Params, <<"create authority">>),
    domain_pers:create_authority(Params).

update_authority(Params) ->
    ?evman_args(Params, <<"update authority">>),
    domain_pers:update_authority(Params).

delete_authority(Params) ->
    ?evman_args(Params, <<"delete authority">>),
    domain_pers:delete_authority(Params).


%% ---------------------------------------------------------------------------
%% Внутрениие функции
%% ---------------------------------------------------------------------------
