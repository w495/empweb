%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_pers
-module(empdb_biz_pers).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

-include("empdb.hrl").

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
    count/1,
    get_opt/2,
    get_opt/3
]).

%%
%% Друзья пользователя
%%
-export([
    get_friend/1,
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
    case create__(Pass, Params) of
        {ok, Persobj} ->
            [{Perspl}|_] = Persobj,
            Id = proplists:get_value(id, Perspl),
            %% создаем запись в базе jabberd
            %case empdb_dao_pers:create_ejabberd(ejabberd, [
            case empdb_dao_pers:create_ejabberd(ejabberd, [
                {username, empdb_convert:to_list(Id)},
                {password, Pass}
            ]) of
                {ok, _}->
                    {ok, Persobj};
                {error,{not_unique,<<"users">>}} ->
                    case empdb_dao_pers:update_ejabberd(ejabberd, [
                        {filter, [{username, empdb_convert:to_list(Id)}]},
                        {password, Pass}
                    ]) of
                        {ok, _}->
                            {ok, Persobj};
                        {Eclass, Error} ->
                            {Eclass, Error}
                    end;
                {Eclass, Error} ->
                    {Eclass , Error}
            end;
        {Eclass, Error} ->
            {Eclass, Error}
    end.


suggest_nick(Con, Orgnick)->
    lists:sort(
        fun(X, Y) ->
            erlang:byte_size(X) < erlang:byte_size(Y)
        end,
        lists:sort(
            lists:filter(
                fun(Nick)->
                    case empdb_dao_pers:get(Con, [{nick, Nick}]) of
                        {ok, []} ->
                            true;
                        _ ->
                            false
                    end
                end,
                empdb_suggest:string(Orgnick, [
                    {postwords, [
                        <<"s">>,
                        <<"us">>,
                        <<"er">>,
                        <<"me">>,
                        <<"man">>,
                        <<"nick">>,
                        <<"user">>
                    ]},
                    {stopwords, [
                        <<"man">>,
                        <<"nick">>,
                        <<"user">>
                    ]}
                ])
            )
        )
    ).


create__(Pass, Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        case empdb_dao_pers:create(Con, [{phash, phash(Pass)}, {fields, [id, login, nick]}|Params]) of
            {ok, Persobj}->
                [{Perspl}|_] = Persobj,
                case {
                    empdb_dao_blog:create(Con, [
                        {owner_id,  proplists:get_value(id, Perspl)},
                        {head, null},
                        {body, null}
                    ]),
                    empdb_dao_album:create(Con, [
                        {owner_id,  proplists:get_value(id, Perspl)},
                        {head, null},
                        {body, null}
                    ])
                } of
                    {   {ok, [{Docpl}]},
                        {ok, [{Albpl}]}
                    } ->
                        {ok, [{[
                            {blog_id,   proplists:get_value(id, Docpl)},
                            {album_id,  proplists:get_value(id, Albpl)}
                            |Perspl
                        ]}]};
                    {{Eclass, Error}, _} ->
                        {Eclass, Error};
                    {_, {Eclass, Error}} ->
                        {Eclass, Error}
                end;
            {error,{not_unique,<<"nick">>}}->
                    Nick = proplists:get_value(nick, Params),
                    Sugs = suggest_nick(Con, Nick),
                {error,{not_unique_nick,Sugs}};
            {Eclass, Error} ->
                {Eclass, Error}
        end
    end).

    
%%
%% @doc Обновляет пользователя. Если у пользователя указан пароль,
%% то обновляется хешь пароля в базе данных сервера приложений,
%% и обновляется запись пользователя сервера jabberd.
%%
update(Params)->
    case proplists:get_value(pass, Params) of
        undefined ->
            %% не пытаемся поменять пароль
            update_(emp, Params);
        Mbpass ->
            %% пытаемся поменять пароль
            case update_(emp, [
                {phash, phash(Mbpass)}
                |Params
            ])  of
                {ok, Persobj} ->
                    [{Perspl}|_] = Persobj,
                    Id = proplists:get_value(id, Perspl),
                    %% изменяем запись в базе jabberd
                    case empdb_dao_pers:update_ejabberd(ejabberd, [
                        {filter, [{username, empdb_convert:to_list(Id)}]},
                        {password, Mbpass}
                    ]) of
                        {ok, _}->
                            {ok, Persobj};
                        {Eclass, Error} ->
                            {Eclass, Error}
                    end;
                {Eclass, Error} ->
                    {Eclass, Error}
            end
    end.

update_(Con, Params)->
    empdb_dao_pers:update(Con, Params).


%%
%% @doc Вход пользователя. Создание сессии.
%% Сначала определяет, какой из параметров был передан, 
%% и логинит пользователя по этому параметру.
%%
login(Params) ->
    io:format("Params = ~p ~n~n~n~n", [Params]),

    
    Id      = proplists:get_value(id,       Params),
    Login   = proplists:get_value(login,    Params),
    Nick    = proplists:get_value(nick,    Params),
    if
        Id =/= undefined ->
            login({id, Id}, Params);
        Login =/= undefined ->
            login({login, Login}, Params);
        Nick =/= undefined ->
            login({nick, Nick}, Params);
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
    empdb_dao:with_transaction(emp, fun(Con)->
        io:format("Params = ~p ~n~n~n~n", [Params]),
        case empdb_dao_pers:get(Con, [{isdeleted, false}|Params]) of
            {ok, [{Userpl}]} ->
                {ok, Perm_list} = empdb_dao_pers:get_perm(Con, Params, [alias]),
                Perm_names = lists:map(fun({Permpl})->
                    empdb_convert:to_atom(proplists:get_value(alias, Permpl))
                end, Perm_list),
                Phash = proplists:get_value(phash, Userpl),
                ?empdb_debug("Userpl   = ~p~n~n", [Userpl]),
                ?empdb_debug("Phash   = ~p~n~n", [Phash]),
                ?empdb_debug("Mbphash = ~p~n~n", [Mbphash]),
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
                        spawn_link(fun()->
                            %% Ключевой момент: без spawn_link код ниже может 
                            %% привести к блокировкам, а так, 
                            %% он выполняется независимо.
                            case proplists:get_value(pstatus_alias, Userpl) of
                                <<"offline">> ->
                                    empdb_dao:with_transaction(emp, fun(Con1) ->
                                        %%
                                        %% Ставим пользователю статус online
                                        %%
                                        empdb_dao_pers:update(Con1, [
                                            {pstatus_alias, <<"online">>}
                                            |Params
                                        ])
                                    end);
                                _ ->
                                    ok
                            end
                        end),
                        %%
                        %% Получаем блог пользователя.
                        %%
                        {ok, [Blog]} =
                            case empdb_dao_blog:get_adds(Con, empdb_dao_blog:get(Con, [
                                {owner_id, proplists:get_value(id, Userpl)},
                                {limit, 1}
                            ], [
                                vcounter,
                                nprotectedposts,
                                nprivateposts,
                                npublicposts,
                                ncomments,
                                nposts,
                                contype_alias,
                                contype_id,
                                comm_acctype_alias,
                                comm_acctype_id,
                                read_acctype_alias,
                                read_acctype_id,
                                contype_id,
                                contype_alias,
                                id
                            ])) of
                                {ok, []} -> {ok, [null]};
                                Res1 -> Res1
                            end,
                            
                        {ok, [Album]} =
                            case empdb_dao_album:get_adds(Con, empdb_dao_album:get(Con, [
                                {owner_id, proplists:get_value(id, Userpl)},
                                {limit, 1}
                            ], [
                                vcounter,
                                nprotectedposts,
                                nprivateposts,
                                npublicposts,
                                ncomments,
                                nposts,
                                contype_alias,
                                contype_id,
                                comm_acctype_alias,
                                comm_acctype_id,
                                read_acctype_alias,
                                read_acctype_id,
                                contype_id,
                                contype_alias,
                                id
                            ])) of
                                {ok, []} -> {ok, [null]};
                                Res2 -> Res2
                            end,
                        %%
                        %% Получаем комнату пользователя
                        %%
                        {ok, [Live_room]} =
                            empdb_dao_room:get(Con, [
                                {id, proplists:get_value(live_room_id, Userpl)},
                                {limit, 1}
                            ], [
                                id,
                                head,
                                body,
                                roomtype_id,
                                roomtype_alias,
                                ulimit,
                                chatlang_id,
                                chatlang_alias,
                                regimen_id,
                                regimen_alias,
                                topic_id,
                                slogan,
                                weather,
                                treas
                            ]),
                        Live_community =
                            case empdb_dao_community:get(Con, [
                                {id, proplists:get_value(live_community_id, Userpl)},
                                {limit, 1},
                                {fields, [
                                    ncands,
                                    nmembs,
                                    head,
                                    id,
                                    communitytype_id,
                                    communitytype_alias,
                                    read_acctype_id,
                                    read_acctype_alias,
                                    comm_acctype_id,
                                    comm_acctype_alias,
                                    contype_id,
                                    contype_alias,
                                    vcounter
                                ]}
                            ]) of
                                {ok, [Community1]} ->
                                    Community1;
                                {ok, []} ->
                                    null
                            end,
                        {ok,[{[{count,Nfriends}]}]} =
                            empdb_dao_friend:count(Con, [
                                {pers_id, proplists:get_value(id, Userpl)}
                            ]),
                        {ok,[{[{count,Nnewmessages}]}]} =
                            empdb_dao_message:count(Con,[
                                {filter, [
                                    {isdfr,         false},
                                    {isdeleted,     false},
                                    {oktype_alias,  null},
                                    {reader_id, proplists:get_value(id, Userpl)}
                                ]}
                            ]),
                        {ok, [{[
                            {nnewmessages,      Nnewmessages},
                            {nfriends,          Nfriends},
                            {perm_names,        Perm_names},
                            {blog,              Blog},
                            {album,             Album},
                            {live_community,    Live_community},
                            {live_room,         Live_room}
                            |Userpl
                        ]}]}
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
    empdb_dao:with_transaction(emp, fun(Con)->
        {ok,[{[{id,Pstatus_id}]}]} = 
            empdb_dao_pers:get_pstatus(Con, [{alias, offline}], [id]),
        empdb_dao_pers:update(Con, [{pstatus_id, Pstatus_id}|Params]),
        empdb_dao_pers:get(Con, [{isdeleted, false}|Params])
    end).

get(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get(Con, [{isdeleted, false}|Params])
    end).


count(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:count(Con, [{isdeleted, false}|Params])
    end).

get(Params, Fileds)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get(Con, [{isdeleted, false}|Params], Fileds)
    end).


get_opt(Params, Options)->
    empdb_dao:with_connection(emp, fun(Con)->
        {ok, Userpls} = empdb_dao_pers:get(Con, [{isdeleted, false}|Params]),
        get_opt(Con, Params, Options, Userpls)
    end).

get_opt(Params, Fileds, Options)->
    empdb_dao:with_connection(emp, fun(Con)->
        {ok, Userpls} = empdb_dao_pers:get(Con, [{isdeleted, false}|Params], Fileds),
        get_opt(Con, Params, Options, Userpls)
    end).

get_opt(Con, Params, [], Proplist)
    -> {ok, Proplist};

get_opt(Con,Params, [Option|Options], [{Acc}])->
    ?empdb_debug("Option = ~p~n", [Option]),
    case Option of
        nfriends ->
            case {proplists:get_value(id, Params), proplists:get_value(nick, Params)} of
                {undefined, undefined} ->
                    get_opt(Con, Params, Options, [{Acc}]);
                {undefined, Nick} ->
                    {ok,[{[{count,Nfriends}]}]} = empdb_dao_friend:count(Con, [
                        {pers_nick, Nick}
                    ]),
                    get_opt(Con, Params, Options, [{[{nfriends, Nfriends}|Acc]}]);
                {Id, _} ->
                    {ok,[{[{count,Nfriends}]}]} = empdb_dao_friend:count(Con, [
                        {pers_id, Id}
                    ]),
                    get_opt(Con, Params, Options, [{[{nfriends, Nfriends}|Acc]}])
            end;
        blog ->
            case {proplists:get_value(id, Params), proplists:get_value(nick, Params)} of
                {undefined, undefined} ->
                    get_opt(Con, Params, Options, [{Acc}]);
                {Id, Nick} ->
                    case empdb_dao_blog:get_adds(Con, empdb_dao_blog:get(Con, [
                        {'or', [
                            {owner_id,      Id},
                            {owner_nick,    Nick}
                        ]},
                        {limit, 1},
                        {fields, [
                            nposts,
                            npublicposts,
                            nprotectedposts,
                            ncomments,
                            id,
                            read_acctype_id,
                            read_acctype_alias,
                            comm_acctype_id,
                            comm_acctype_alias,
                            contype_id,
                            contype_alias,
                            vcounter
                        ]}
                    ])) of
                        {ok, [Blog|_]} ->
                            get_opt(Con, Params, Options, [{[{blog, Blog}|Acc]}]);
                        _ ->
                            get_opt(Con, Params, Options, [{[{blog, null}|Acc]}])
                    end
            end;
        album ->
            case {proplists:get_value(id, Params), proplists:get_value(nick, Params)} of
                {undefined, undefined} ->
                    get_opt(Con, Params, Options, [{Acc}]);
                {Id, Nick} ->
                    case empdb_dao_album:get_adds(Con, empdb_dao_album:get(Con, [
                        {'or', [
                            {owner_id,      Id},
                            {owner_nick,    Nick}
                        ]},
                        {limit, 1},
                        {fields, [
                            nposts,
                            npublicposts,
                            nprotectedposts,
                            ncomments,
                            id,
                            read_acctype_id,
                            read_acctype_alias,
                            comm_acctype_id,
                            comm_acctype_alias,
                            contype_id,
                            contype_alias,
                            vcounter
                        ]}
                    ])) of
                        {ok, [Album|_]} ->
                            get_opt(Con, Params, Options, [{[{album, Album}|Acc]}]);
                        _ ->
                            get_opt(Con, Params, Options, [{[{album, null}|Acc]}])
                    end
            end;
        community ->
            case {proplists:get_value(id, Params), proplists:get_value(nick, Params)} of
                {undefined, undefined} ->
                    get_opt(Con, Params, Options, [{Acc}]);
                {Id, Nick} ->
                    case empdb_dao_community:get(Con, [
                        {'or', [
                            {owner_id,      Id},
                            {owner_nick,    Nick}
                        ]},
                        {limit, 1},
                        {fields, [
                            ncands,
                            nmembs,
                            head,
                            id,
                            communitytype_id,
                            communitytype_alias,
                            read_acctype_id,
                            read_acctype_alias,
                            comm_acctype_id,
                            comm_acctype_alias,
                            contype_id,
                            contype_alias,
                            vcounter
                        ]}
                    ]) of
                        {ok, [Community|_]} ->
                            get_opt(Con, Params, Options, [{[{community, Community}|Acc]}]);
                        _ ->
                            get_opt(Con, Params, Options, [{[{community, null}|Acc]}])
                    end
            end;
        without_phash ->
            Nacc = proplists:delete(phash,
                proplists:delete(pass, Acc)
            ),
            get_opt(Con, Params, Options, [{Nacc}]);
        %% ------------------------------------------------------------------
        {perm_list, Spec} when erlang:is_list(Spec) ->
            {ok, Perm_list} = empdb_dao_pers:get_perm(Con, Params, Spec),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);
        {perm_list, Spec} when erlang:is_atom(Spec) ->
            {ok, Perm_list} = empdb_dao_pers:get_perm(Con, Params, [Spec]),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);
        perm_list ->
            {ok, Perm_list} = empdb_dao_pers:get_perm(Con, Params, [alias]),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);
        perm_names ->
            {ok, Perm_list} = empdb_dao_pers:get_perm(Con, Params, [alias]),
            Perm_names = lists:map(fun({Permpl})->
                empdb_convert:to_atom(proplists:get_value(alias, Permpl))
            end, Perm_list),
            get_opt(Con, Params, Options, [{[{perm_names, Perm_names}|Acc]}]);
        %% ------------------------------------------------------------------
        {group_list, Spec} when erlang:is_list(Spec) ->
            {ok, Perm_list} = empdb_dao_pers:get_group(Con, Params, Spec),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);
        {group_list, Spec} when erlang:is_atom(Spec) ->
            {ok, Perm_list} = empdb_dao_pers:get_group(Con, Params, [Spec]),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);
        group_list ->
            {ok, Perm_list} = empdb_dao_pers:get_group(Con, Params, [alias]),
            get_opt(Con, Params, Options, [{[{perm_list, Perm_list}|Acc]}]);    
        {room_, Fields} ->
            Roompl_ = case empdb_dao_room:get(Con, 
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
    empdb_dao:with_connection(emp, fun(Con)->
    
        Pers_id = proplists:get_value(pers_id, Params,
            case empdb_dao_pers:get(Con,
                [   {isdeleted, false},
                    {nick, proplists:get_value(pers_nick, Params, [])},
                    {limit, 1}
                ], [id]
            ) of
                {ok, [{[{id, Id1}]}]} ->
                    Id1;
                Error1 ->
                    Error1
            end
        ),
        Friend_id = proplists:get_value(friend_id, Params,
            case empdb_dao_pers:get(Con,
                [   {isdeleted, false},
                    {nick, proplists:get_value(friend_nick, Params, [])},
                    {limit, 1}
                ], [id]
            ) of
                {ok, {[{id, Id2}]}} ->
                    Id2;
                Error2 ->
                    Error2
            end
        ),
        case empdb_dao_pers:add_friend(Con, [
            {pers_id,   Pers_id},
            {friend_id, Friend_id}
        ]) of
            {ok, _frndpls} ->
                %% TODO: костыль
                %% Возможно, иммет смысл получать 
                %% на основе _frndpls
                empdb_dao_pers:get(Con,
                    [{isdeleted, false},
                    {id, proplists:get_value(friend_id, Params)}],
                    [   id,
                        nick,
                        pstatus_id,
                        pstatus_alias,
                        live_room_id,
                        live_room_head
                    ]
                );
            Error ->
                Error
        end
    end).

delete_friend(Params)->
    io:format("~n~n~n~nParams = ~p~n~n~n~n", [Params]),
    empdb_dao:with_connection(emp, fun(Con)->
        Pers_id = proplists:get_value(pers_id, Params,
            case empdb_dao_pers:get(Con,
                [   {isdeleted, false},
                    {nick, proplists:get_value(pers_nick, Params, [])},
                    {limit, 1}
                ], [id]
            ) of
                {ok, [{[{id, Id1}]}]} ->
                    Id1;
                Error ->
                    Error
            end
        ),
        Friend_id = proplists:get_value(friend_id, Params,
            case empdb_dao_pers:get(Con,
                [   {isdeleted, false},
                    {nick, proplists:get_value(friend_nick, Params, [])},
                    {limit, 1}
                ], [id]
            ) of
                {ok, {[{id, Id2}]}} ->
                    Id2;
                Error2 ->
                    Error2
            end
        ),
        empdb_dao_pers:delete_friend(Con, [
            {pers_id,   Pers_id},
            {friend_id, Friend_id}
        ])
    end).

get_friend(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_friend:get(Con, [
            {order, {nick, asc}}
            |Params
        ])
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Эмоции пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_emotion(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_emotion(Con, [{isdeleted, false}|Params])
    end).

get_emotion(Params, Fileds)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_emotion(Con, [{isdeleted, false}|Params], Fileds)
    end).

update_emotion(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:update_emotion(Con, Params)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Группа пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_pgroup(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_pgroup(Con, [{isdeleted, false}|Params])
    end).

get_pgroup(Params, Fileds)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_pgroup(Con, [{isdeleted, false}|Params], Fileds)
    end).

update_pgroup(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:update_pgroup(Con, Params)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Семейное положение пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_mstatus(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_mstatus(Con, [{isdeleted, false}|Params])
    end).

get_mstatus(Params, Fileds)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_mstatus(Con, [{isdeleted, false}|Params], Fileds)
    end).

update_mstatus(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:update_mstatus(Con, Params)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Статус пользователя пользователя: в сети \ не в сети.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_pstatus(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_pstatus(Con, [{isdeleted, false}|Params])
    end).

get_pstatus(Params, Fileds)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_pstatus(Con, [{isdeleted, false}|Params], Fileds)
    end).

update_pstatus(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:update_pstatus(Con, Params)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Авторитет пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_authority(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_authority(Con, [{isdeleted, false}|Params])
    end).

get_authority(Params, Fileds)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_authority(Con, [{isdeleted, false}|Params], Fileds)
    end).

update_authority(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:update_authority(Con, Params)
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

