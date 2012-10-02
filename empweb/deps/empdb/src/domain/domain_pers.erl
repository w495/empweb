%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_pers
-module(domain_pers).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

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
    logout/1,
    get/1,
    get/2,
    get_opt/2,
    get_opt/3
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
%% Группа пользователя
%%
-export([
    get_pgroup/1,
    get_pgroup/2,
    update_pgroup/1
]).

%%
%% Статус пользователя
%%
-export([
    get_pstatus/1,
    get_pstatus/2,
    update_pstatus/1
]).

%%
%% Семейное положение пользователя
%%
-export([
    get_mstatus/1,
    get_mstatus/2,
    update_mstatus/1
]).

%%
%% Авторитет пользователя
%%
-export([
    get_authority/1,
    get_authority/2,
    update_authority/1
]).

%%
%% Эмоции пользователя
%%
-export([
    get_emotion/1,
    get_emotion/2,
    update_emotion/1
]).

%% ===========================================================================
%% Внешние функции
%% ===========================================================================

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Сам пользователь
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc Cоздает нового пользователя в базе данных сервера приложений,
%% и нового пользователя сервера jabberd.
%%
register(Params)->
    Pass = proplists:get_value(pass, Params),
    case dao_pers:create(emp, [{phash, phash(Pass)}|Params]) of
        {ok, Objects} ->
            [{Pl}|_] = Objects,
            Id = proplists:get_value(id, Pl),
            %% создаем запись в базе jabberd
            %case dao_pers:create_ejabberd(ejabberd, [
            case dao_pers:create_ejabberd(ejabberd, [
                {username, convert:to_list(Id)},
                {password, Pass}
            ]) of
                {ok, _}->
                    {ok, Objects};
                {error,{not_unique,<<"users">>}} ->
                    case dao_pers:update_ejabberd(ejabberd, [
                        {filter, [{username, convert:to_list(Id)}]},
                        {password, Pass}
                    ]) of
                        {ok, _}->
                            {ok, Objects};
                        {Eclass, Error} ->
                            {Eclass, Error}
                    end;
                {Eclass, Error} ->
                    {Eclass , Error}
            end;
        {Eclass, Error} ->
            {Eclass, Error}
    end.

%%
%% @doc Обновляет пользователя. Если у пользователя указан пароль,
%% то обновляется хешь пароля в базе данных сервера приложений,
%% и обновляется запись пользователя сервера jabberd.
%%
update(Params)->
    case proplists:get_value(pass, Params) of
        undefined ->
            %% не пытаемся поменять пароль
            dao_pers:update(emp, Params);
        Mbpass ->
            %% пытаемся поменять пароль
            case dao_pers:update(emp, [
                {phash, phash(Mbpass)}
                |Params
            ])  of
                {ok, Objects} ->
                    [{Pl}|_] = Objects,
                    Id = proplists:get_value(id, Pl),
                    %% изменяем запись в базе jabberd
                    case dao_pers:update_ejabberd(ejabberd, [
                        {username, convert:to_list(Id)},
                        {password, Mbpass}
                    ]) of
                        {ok, _}->
                            {ok, Objects};
                        {Eclass, Error} ->
                            {Eclass, Error}
                    end;
                {Eclass, Error} ->
                    {Eclass, Error}
            end
    end.

%%
%% @doc Вход пользователя. Создание сессии.
%% Сначала определяет, какой из параметров был передан, 
%% и логинит пользователя по этому параметру.
%%
login(Params) ->
    Id      = proplists:get_value(id,       Params),
    Login   = proplists:get_value(login,    Params),
    if
        Id =/= undefined ->
            login({id, Id}, Params);
        Login =/= undefined ->
            login({login, Login}, Params);
        true ->
            {error,{bad_pers,{Params}}}
    end.

%%
%% @doc Проверяет, соответвует ли id или логин пользователя его паролю.
%% Выдает ошибку, если такого пользователя нет или если пароль не соответвует
%%

login({Uf, Uv}, Params) ->
    Mbpass = proplists:get_value(pass, Params),
    Mbphash = phash(Mbpass),
    %% Фиктиные переменные, в логике реально не участвуют.
    %% Нужны для быстрого построения проверки, 
    %% на количество ошибочных логинов.
    Max_auth_error = 10,    
    EC = 0,
    dao:with_transaction(emp, fun(Con)->
        case dao_pers:get(Con, [{isdeleted, false}|Params]) of
            {ok, [{Userpl}]} ->
                {ok, Perm_list} = dao_pers:get_perm(Con, Params, [alias]),
                Perm_names = lists:map(fun({Permpl})->
                    convert:to_atom(proplists:get_value(alias, Permpl))
                end, Perm_list),
                Phash = proplists:get_value(phash, Userpl),
                io:format("Userpl   = ~p~n~n", [Userpl]),
                io:format("Phash   = ~p~n~n", [Phash]),
                io:format("Mbphash = ~p~n~n", [Mbphash]),
                case {Phash =/= Mbphash, Max_auth_error - (EC + 1) > 0} of
                    {true, false} ->
                        %% Фиктиная ветка, в реалности не выполняется.
                        %% Нужны для быстрого построения проверки,
                        %% на количество ошибочных логинов.
                        {error, {auth_count_overflow,
                            {[
                                {max,  Max_auth_error - (EC + 1)},
                                {Uf, Uv},
                                {pass, Mbpass}
                            ]}
                        }};
                    {true, _} ->
                        %% Есть такой пользователь, но нет такого пароля.
                        {error, {bad_password,
                                {[
                                    {max,  Max_auth_error - (EC + 1)},
                                    {Uf, Uv},
                                    {pass, Mbpass}
                                ]}
                        }};
                    _ ->
                        {ok,[{[{id,Pstatus_id}]}]} = dao_pers:get_pstatus(Con, [{alias, online}], [id]),
                        dao_pers:update(Con, [{pstatus_id, Pstatus_id}|Params]),
                        {ok, [{[{perm_names, Perm_names}|Userpl]}]}
                end;
            _ ->
                %% Нет такого пользователя
                {error,
                    {bad_pers,
                        {[
                            {Uf,    Uv},
                            {pass,  Mbpass}
                        ]}
                    }
                }
        end
    end).
    
logout(Params)->
    dao:with_transaction(emp, fun(Con)->
        {ok,[{[{id,Pstatus_id}]}]} = 
            dao_pers:get_pstatus(Con, [{alias, offline}], [id]),
        dao_pers:update(Con, [{pstatus_id, Pstatus_id}|Params]),
        dao_pers:get(Con, [{isdeleted, false}|Params])
    end).

get(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get(Con, [{isdeleted, false}|Params], Fileds)
    end).


get_opt(Params, Options)->
    dao:with_connection(emp, fun(Con)->
        {ok, Userpls} = dao_pers:get(Con, [{isdeleted, false}|Params]),
        get_opt(Con, Params, Options, Userpls)
    end).

get_opt(Params, Fileds, Options)->
    dao:with_connection(emp, fun(Con)->
        {ok, Userpls} = dao_pers:get(Con, [{isdeleted, false}|Params], Fileds),
        get_opt(Con, Params, Options, Userpls)
    end).

get_opt(Con, Params, [], Proplist)
    -> {ok, Proplist};

get_opt(Con,Params, [Option|Options], [{Acc}])->
    io:format("Option = ~p~n", [Option]),
    case Option of
        %% ------------------------------------------------------------------
        {perm_list, Spec} when erlang:is_list(Spec) ->
            {ok, Perm_list} = dao_pers:get_perm(Con, Params, Spec),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);
        {perm_list, Spec} when erlang:is_atom(Spec) ->
            {ok, Perm_list} = dao_pers:get_perm(Con, Params, [Spec]),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);
        perm_list ->
            {ok, Perm_list} = dao_pers:get_perm(Con, Params, [alias]),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);
        perm_names ->
            {ok, Perm_list} = dao_pers:get_perm(Con, Params, [alias]),
            Perm_names = lists:map(fun({Permpl})->
                convert:to_atom(proplists:get_value(alias, Permpl))
            end, Perm_list),
            get_opt(Con, Params, Options, [{[{perm_names, Perm_names}|Acc]}]);
        %% ------------------------------------------------------------------
        {group_list, Spec} when erlang:is_list(Spec) ->
            {ok, Perm_list} = dao_pers:get_group(Con, Params, Spec),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);
        {group_list, Spec} when erlang:is_atom(Spec) ->
            {ok, Perm_list} = dao_pers:get_group(Con, Params, [Spec]),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);
        group_list ->
            {ok, Perm_list} = dao_pers:get_group(Con, Params, [alias]),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);    
        {room_, Fields} ->
            Roompl_ = case dao_room:get(Con, 
                [{id, proplists:get_value(room_id, Acc, null)}], 
                Fields
            ) of
                {ok, [{Roompl}]} ->
                    lists:map(fun({Key, Value})->
                        {list_to_atom("room_" ++ atom_to_list(Key)), Value} 
                    end, Roompl);
                _ -> 
                    []
            end,
            get_opt(Con, Params, Options, [{lists:append(Roompl_, Acc)}]);
        _ ->
            get_opt(Con, Params, Options, [{Acc}])
    end;

get_opt(Con,Params, [Option|Options], Accs)->
    {ok,
        lists:map(fun({Obj})->
            {ok, [{Result}]} = get_opt(Con,Params, [Option|Options], [{Obj}]),
            {Result}
        end, Accs
    )}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Друзья пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_friend(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:add_friend(Con, Params)
    end).

delete_friend(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:delete_friend(Con, Params)
    end).

get_friends(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get_friends(Con, Params)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Эмоции пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_emotion(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get_emotion(Con, [{isdeleted, false}|Params])
    end).

get_emotion(Params, Fileds)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get_emotion(Con, [{isdeleted, false}|Params], Fileds)
    end).

update_emotion(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:update_emotion(Con, Params)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Группа пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_pgroup(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get_pgroup(Con, [{isdeleted, false}|Params])
    end).

get_pgroup(Params, Fileds)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get_pgroup(Con, [{isdeleted, false}|Params], Fileds)
    end).

update_pgroup(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:update_pgroup(Con, Params)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Семейное положение пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_mstatus(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get_mstatus(Con, [{isdeleted, false}|Params])
    end).

get_mstatus(Params, Fileds)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get_mstatus(Con, [{isdeleted, false}|Params], Fileds)
    end).

update_mstatus(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:update_mstatus(Con, Params)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Статус пользователя пользователя: в сети \ не в сети.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_pstatus(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get_pstatus(Con, [{isdeleted, false}|Params])
    end).

get_pstatus(Params, Fileds)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get_pstatus(Con, [{isdeleted, false}|Params], Fileds)
    end).

update_pstatus(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:update_pstatus(Con, Params)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Авторитет пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_authority(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get_authority(Con, [{isdeleted, false}|Params])
    end).

get_authority(Params, Fileds)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:get_authority(Con, [{isdeleted, false}|Params], Fileds)
    end).

update_authority(Params)->
    dao:with_connection(emp, fun(Con)->
        dao_pers:update_authority(Con, Params)
    end).

%% ===========================================================================
%% Внутренние функции
%% ===========================================================================

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

