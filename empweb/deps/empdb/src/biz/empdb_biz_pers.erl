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
    get_opt/3,
    wfoe/2
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
%% Чиновничий Статус пользователя
%%
-export([
    get_ostatus/1,
    get_ostatus/2,
    update_ostatus/1
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



wfoe(Function, Options)->
    Pers_id     = proplists:get_value(pers_id,      Options),
    Pers_nick   = proplists:get_value(pers_nick,    Options),
    Friend_id   = proplists:get_value(friend_id,    Options),
    Friend_nick = proplists:get_value(friend_nick,  Options),
    fun(Connection) ->
        {ok, Objs} = empdb_dao_friend:get(Connection, [
            {'or', [
                {pers_id,   Pers_id},
                {pers_nick, Pers_nick}
            ]},
            {'or', [
                {friend_id,     Friend_id},
                {friend_nick,   Friend_nick}
            ]},
            {friendtype_alias, foe}
        ]),
        case Objs of
            [] ->
                Function(Connection);
            _ ->
                {error, forbiden}
        end
    end.

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
    io:format("~n~n~n~nOrgnick = ~p ~n~n~n~n", [Orgnick]),
    lists:sort(
        fun(X, Y) ->
            erlang:byte_size(X) < erlang:byte_size(Y)
        end,
        lists:sort(
            lists:filter(
                fun(Nick)->
                    case empdb_dao_pers:get(Con, [{nick, Nick}], [nick]) of
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
            update_(Params);
        Mbpass ->
            %% пытаемся поменять пароль
            case update_([
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

update_(Params)->
    empdb_dao:with_transaction(fun(Con)->
        Pers =
            empdb_dao_pers:get(Con,[
                {id,    proplists:get_value(id,   Params)}
            ]),
        case Pers of
            {ok, [{Mbperspl}]} ->
                Fun = lists:foldl(
                    fun({Key, Value}, Accfun)->
                        fun(Con1, Params1) ->
                            update(Con1, {Key, Value},  {Accfun, [Params1]}, Mbperspl)
                        end
                    end,
                    fun empdb_dao_pers:update/2,
                    Params
                ),
                Fun(Con, Params);
            Else ->
                Else
        end
    end).

update(Con, {nick, undefined},  {Function, [Params]}, _mbperspl) ->
    Function(Con, Params);

update(Con, {nick, Nick},  {Function, [Params]}, Mbperspl) ->
    Price = 1.0,
    Money = proplists:get_value(money, Mbperspl),
    Oldnick = proplists:get_value(nick, Mbperspl),
    case {Price =< Money, Oldnick =:= Nick} of
        {true, false} ->
            case Function(Con, Params) of
                {ok, [{Item}]} ->
                    {ok, _} =
                        empdb_dao_pay:create(Con, [
                            {pers_id,           proplists:get_value(id,   Item)},
                            {paytype_alias,     change_nick},
                            {isincome,          false},
                            {price,             Price}
                        ]),
                    {ok, _} =
                        empdb_dao_pers:update(Con,[
                            {id,    proplists:get_value(id,   Item)},
                            {money, {decr, Price}}
                        ]),
                    Itemid = proplists:get_value(id,   Item),
                    update_change_connected(Con, doc, owner, Itemid, Nick),
                    update_change_connected(Con, pay, pers, Itemid, Nick),
                    update_change_connected(Con, vote, pers, Itemid, Nick),
                    update_change_connected(Con, file, owner, Itemid, Nick),
                    update_change_connected(Con, fileinfo, owner, Itemid, Nick),
                    update_change_connected(Con, communityhist, pers, Itemid, Nick),
                    update_change_connected(Con, message, reader, Itemid, Nick),
                    update_change_connected(Con, roombet, owner, Itemid, Nick),
                    update_change_connected(Con, room, roombet, Itemid, Nick),
                    update_change_connected(Con, thingbuy, buyer, Itemid, Nick),
                    update_change_connected(Con, thingbuy, owner, Itemid, Nick),
                    update_change_connected(Con, experbuy, buyer, Itemid, Nick),
                    update_change_connected(Con, experbuy, owner, Itemid, Nick),
                    update_change_connected(Con, rptrans, pers, Itemid, Nick),
                    update_change_connected(Con, roomtreas, pers, Itemid, Nick),
        %             update_change_connected(Con, thingwish, buyer, Itemid, Nick),
        %             update_change_connected(Con, thingwish, owner, Itemid, Nick),
                    {ok, [{Item}]};
                {error,{not_unique,<<"nick">>}}->
                    Sugs = suggest_nick(Con, Nick),
                    {error,{not_unique_nick,Sugs}};
                Error ->
                    Error
            end;
        {true, _} ->
            {ok, []};
        {false, _} ->
            {error, {not_enough_money, {[
                {money, Money},
                {price, Price}
            ]}}}
    end;

%% 
%% Человек попросился в сообщество, 
%% и стал кандидатом
%% 
update(Con, {live_community_id, Community_id}, {Function, [Params]}, Mbperspl) ->
    case
        (proplists:get_value(live_community_id, Mbperspl) =/= Community_id)
            and
        (Community_id =/= null)
    of
        true ->
            %% сообщество изменилось
            {ok, [{Communitypl}]} =
                empdb_dao_community:get(Con, [
                    {id, Community_id},
                    {limit, 1},
                    {fields, [
                        cands_gte_authority_level
                    ]}
                ]),
            Candsgteauthoritylevel =
                proplists:get_value(cands_gte_authority_level, Communitypl),
            Authoritylevel =
                proplists:get_value(authority_level, Mbperspl),
            %% если уровень человека ниже уровня сообщества,
            %% то вступить в него он не может.
            case  Candsgteauthoritylevel =< Authoritylevel of
                true ->
                    case Function(Con, Params) of
                        {ok, Res} ->
                            empdb_dao_communityhist:create(Con, [
                                {community_id,
                                    Community_id},
                                {pers_id,
                                    proplists:get_value(id, Mbperspl)},
                                {communityhisttype_alias,
                                    pers_cand}
                            ]),
                            {ok, Res};
                        Else ->
                            Else
                    end;
                _ ->
                    {error, {not_enough_level, {[
                        {cands_gte_authority_level,
                            Candsgteauthoritylevel
                        },
                        {authority_level,
                            Authoritylevel}
                    ]}}}
            end;
        false ->
            %% сообщество НЕ изменилось
            Function(Con, Params)
    end;

%%
%% Человека одобрили как члена сообщества
%% 
update(Con, {live_community_approved, true}, {Function, [Params]}, Mbperspl) ->
    Pers_id   = proplists:get_value(id, Mbperspl),
    case proplists:get_value(live_community_id, Mbperspl) =/= null of
        true ->
            Community_id = proplists:get_value(live_community_id, Mbperspl),
            {ok, [{Communitypl}]} =
                empdb_dao_community:get(Con, [
                    {id, Community_id},
                    {limit, 1},
                    {fields, [
                        fee
                    ]}
                ]),
            Money   = proplists:get_value(money, Mbperspl),
            Price   = proplists:get_value(fee, Communitypl),
            case Price =< Money of
                true ->
                    case Function(Con, Params) of
                        {ok, Res} ->
                            %% Создаем событие истории 
                            %% о том что пользователь стал членом
                            {ok, _} =
                                empdb_dao_communityhist:create(Con, [
                                    {community_id, Community_id },
                                    {pers_id,
                                        proplists:get_value(id, Mbperspl)},
                                    {communityhisttype_alias,
                                        pers_memb}
                                ]),
                            %% Создаем запись в лог казны сообщества
                            {ok, _} =
                                empdb_dao_communitytreas:create(Con, [
                                    {pers_id,           Pers_id},
                                    {community_id,      Community_id},
                                    {isincome,          true},
                                    {treastype_alias,   in},
                                    {price,             Price}
                                ]),
                            %% Создаем запись в лог кошелька пользователя
                            {ok, _} =
                                empdb_dao_pay:create(Con, [
                                    {pers_id,
                                        proplists:get_value(id,   Mbperspl)},
                                    {paytype_alias,     community_out},
                                    {isincome,          false},
                                    {price,             Price}
                                ]),
                            %% Добавляем деньги в общак сообщества
                            {ok, _} =
                                empdb_dao_community:update(Con, [
                                    {id,    Community_id},
                                    {treas, Price}
                                ]),
                            %% Снимаем с пользователя деньги
                            {ok, _} =
                                empdb_dao_pers:update(Con,[
                                    {id,    proplists:get_value(id,   Mbperspl)},
                                    {money, {decr, Price}}
                                ]),
                            {ok, Res};
                        Else ->
                            Else
                    end;
                false ->
                    {error, {not_enough_money, {[
                        {money, Money},
                        {price, Price}
                    ]}}}
            end;
        false ->
            {error, forbiden}
    end;

%%
%% Человека не одобряли как члена сообщества или выгнали.
%%
update(Con, {live_community_approved, _}, {Function, [Params]}, Mbperspl) ->
    case {
        proplists:get_value(live_community_id, Mbperspl) =/= null,
        proplists:get_value(live_community_approved, Mbperspl) =/= true
    } of
        {true, true} ->
            %% Человека не одобряли как члена сообщества
            case Function(Con, Params) of
                {ok, Res} ->
                    empdb_dao_communityhist:create(Con, [
                        {community_id,
                            proplists:get_value(live_community_id, Mbperspl)},
                        {pers_id,
                            proplists:get_value(id, Mbperspl)},
                        {communityhisttype_alias,
                            pers_out}
                    ]),
                    {ok, Res};
                Else ->
                    Else
            end;
        {true, false} ->
            %% Человека выгнали из сообщества
            case Function(Con, [
                {live_community_id, null}
                |proplists:delete(live_community_id, Params)
            ]) of
                {ok, Res} ->
                    empdb_dao_communityhist:create(Con, [
                        {community_id,
                            proplists:get_value(live_community_id, Mbperspl)},
                        {pers_id,
                            proplists:get_value(id, Mbperspl)},
                        {communityhisttype_alias,
                            pers_exile}
                    ]),
                    {ok, Res};
                Else ->
                    Else
            end;
        {false, _} ->
            {error, forbiden}
    end;

    
update(Con, {_pname, _pvalue}, {Function, [Params]}, Mbperspl) ->
    Function(Con, Params).


update_change_connected(Con, Msuffix, Fprefix, Itemid, Nick) ->
    Module = ca(empdb_dao, Msuffix),
    Fid = ca(Fprefix, id),
    Fnick = ca(Fprefix, nick),
    Module:update(Con,[
        {filter, [
            {Fid,   Itemid}
        ]},
        {values, [
            {Fnick, Nick}
        ]}
    ]).

ca(L, R) ->
    erlang:list_to_atom(string:join([erlang:atom_to_list(L), erlang:atom_to_list(R)], "_")).

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
                                {friendtype_alias, friend},
                                {pers_id, proplists:get_value(id, Userpl)}
                            ]),
                        {ok,[{[{count,Nfoes}]}]} =
                            empdb_dao_friend:count(Con, [
                                {friendtype_alias, foe},
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
                            {nfoes,             Nfoes},
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
        {ok,[{Pstatuspl}]} =
            empdb_dao_pers:get_pstatus(Con, [{alias, offline}], [id]),
        Pstatus_id = proplists:get_value(id, Pstatuspl),
        empdb_dao_pers:update(Con, [{pstatus_id, Pstatus_id}|Params]),
        empdb_dao_pers:get(Con, [{isdeleted, false}|Params])
    end).


count(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:count(
            Con,
            [
                {isdeleted, false}
                |get_tfparams(Con, Params)
            ]
        )
    end).

get(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get(
            Con,
            [
                {isdeleted, false}
                |get_tfparams(Con, Params)
            ]
        )
    end).

get(Params, Fields)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get(
            Con,
            [
                {isdeleted, false}
                |get_tfparams(Con, Params)
            ],
            Fields
        )
    end).


get_tfparams(Con, Params) ->
    lists:foldl(
        fun({Key, Value}, Acc)->
            get_tfparams(Con, {Key, Value}, Acc)
        end,
        [],
        Params
    ).

get_tfparams(Con, {geo_id, Geo_id}, Acc1)->
    Geo_ids =
        case empdb_dao_geo:get(emp,
            [
                {parent_id, Geo_id},
                {fields, [id]}
            ]
        ) of
            {ok, Geolist} ->
                lists:foldl(
                    fun({Geoitempl}, Acc)->
                        Id = proplists:get_value(id, Geoitempl),
                        [Id|Acc]
                    end,
                    [Geo_id],
                    Geolist
                );
            Else ->
                [Geo_id]
        end,
    [{geo_id, {in, Geo_ids}}|Acc1];

get_tfparams(Con, {Key, Value}, Acc1)->
    [{Key, Value}|Acc1].



get_opt(Params, Options)->
    empdb_dao:with_connection(emp, fun(Con)->
        {ok, Userpls} = empdb_dao_pers:get(Con, [{isdeleted, false}|get_tfparams(Con, Params)]),
        get_opt(Con, Params, Options, Userpls)
    end).

get_opt(Params, Fields, Options)->
    empdb_dao:with_connection(emp, fun(Con)->
        {ok, Userpls} = empdb_dao_pers:get(Con, [{isdeleted, false}|get_tfparams(Con, Params)], Fields),
        get_opt(Con, [{fields, Fields}|Params], Options, Userpls)
    end).

get_opt(Con, Params, [], Proplist)
    -> {ok, Proplist};

get_opt(Con,Params, [Option|Options], [{Acc}])->
    Fields = proplists:get_value(fields, Params, []),
    io:format(" ~n~n~nOption = ~p ~p ~n~n~n", [Option, Fields]),
    case lists:member(Option, Fields) or (Fields =:= []) of
        true ->
            io:format(" ~n~n~n Option = ~p  ~n~n~n", [Option]),
            case Option of
                friendtype_alias ->
                    io:format(
                        " ~n~n~n                                       "
                        "                       friendtype_alias ~n~n~n", []
                    ),
                    
                    Selfpersid = proplists:get_value(self@pers_id, Params),
                    Friendid = proplists:get_value(id, Acc),
                    Friendnick = proplists:get_value(nick, Acc),
                    io:format(" ~n~n~n _    = ~p  ~n~n~n", [{
                        Selfpersid,
                        Friendid,
                        Friendnick
                    }]),
                     
                    case {Selfpersid, Friendid, Friendnick} of
                        {undefined,
                            _,
                            _
                        } ->
                            get_opt(Con, Params, Options, [{Acc}]);
                        {_,
                            undefined,
                            undefined
                        } ->
                            get_opt(Con, Params, Options, [{Acc}]);
                        {Selfpersid,
                            Friendid,
                            Friendnick
                        } ->
                            case empdb_dao:get(empdb_dao_friend, emp, [
                                {pers_id,   Selfpersid},
                                {'or', [
                                    {friend_id, Friendid},
                                    {friend_nick, Friendnick}
                                ]},
                                {fields, [
                                    friendtype_alias,
                                    friendtype_id
                                ]}
                            ]) of
                                {ok,[]} ->
                                    get_opt(Con, Params, Options, [{[
                                        % {friendtype_id,     Friendtype_id},
                                        {friendtype_alias,  null}
                                        |Acc
                                    ]}]);
                                {ok,[{Friend}]} ->
                                    Friendtype_id =
                                        proplists:get_value(friendtype_id, Friend),
                                    Friendtype_alias =
                                        proplists:get_value(friendtype_alias, Friend),
                                    get_opt(Con, Params, Options, [{[
                                        % {friendtype_id,     Friendtype_id},
                                        {friendtype_alias,  Friendtype_alias}
                                        |Acc
                                    ]}])
                            end
                    end;
                nfriends ->
                    case {proplists:get_value(id, Params), proplists:get_value(nick, Params)} of
                        {undefined, undefined} ->
                            get_opt(Con, Params, Options, [{Acc}]);
                        {undefined, Nick} ->
                            {ok,[{[{count,Nfriends}]}]} = empdb_dao_friend:count(Con, [
                                {friendtype_alias, friend},
                                {pers_nick, Nick}
                            ]),
                            get_opt(Con, Params, Options, [{[{nfriends, Nfriends}|Acc]}]);
                        {Id, _} ->
                            {ok,[{[{count,Nfriends}]}]} = empdb_dao_friend:count(Con, [
                                {friendtype_alias, friend},
                                {pers_id, Id}
                            ]),
                            get_opt(Con, Params, Options, [{[{nfriends, Nfriends}|Acc]}])
                    end;
                nfoes ->
                    case {proplists:get_value(id, Params), proplists:get_value(nick, Params)} of
                        {undefined, undefined} ->
                            get_opt(Con, Params, Options, [{Acc}]);
                        {undefined, Nick} ->
                            {ok,[{[{count,Nfoes}]}]} = empdb_dao_friend:count(Con, [
                                {friendtype_alias, foe},
                                {pers_nick, Nick}
                            ]),
                            get_opt(Con, Params, Options, [{[{nfoes, Nfoes}|Acc]}]);
                        {Id, _} ->
                            {ok,[{[{count,Nfoes}]}]} = empdb_dao_friend:count(Con, [
                                {friendtype_alias, foe},
                                {pers_id, Id}
                            ]),
                            get_opt(Con, Params, Options, [{[{nfoes, Nfoes}|Acc]}])
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
        false ->
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
        % Pers_id = proplists:get_value(pers_id, Params,
        %     case empdb_dao_pers:get(Con,
        %         [   {isdeleted, false},
        %             {nick, proplists:get_value(pers_nick, Params, [])},
        %             {limit, 1}
        %         ], [id]
        %     ) of
        %         {ok, [{[{id, Id1}]}]} ->
        %             Id1;
        %         Error1 ->
        %             Error1
        %     end
        % ),
        %         = proplists:get_value(friend_id, Params,
        %     case empdb_dao_pers:get(Con,
        %         [   {isdeleted, false},
        %             {nick, proplists:get_value(friend_nick, Params, [])},
        %             {limit, 1}
        %         ], [id]
        %     ) of
        %         {ok, {[{id, Id2}]}} ->
        %             Id2;
        %         Error2 ->
        %             Error2
        %     end
        % ),
        case empdb_dao_friend:create(Con, Params) of
            {ok, _frndpls} ->
                %% TODO: костыль
                %% Возможно, иммет смысл получать 
                %% на основе _frndpls
                empdb_dao_pers:get(Con,
                    [   {isdeleted, false},
                        {'or', [
                            {id, proplists:get_value(friend_id, Params)},
                            {nick, proplists:get_value(friend_nick, Params)}
                        ]}
                    ],
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
    empdb_dao:with_connection(emp, fun(Con)->
        % Pers_id = proplists:get_value(pers_id, Params,
        %     case empdb_dao_pers:get(Con,
        %         [   {isdeleted, false},
        %             {nick, proplists:get_value(pers_nick, Params, [])},
        %             {limit, 1}
        %         ], [id]
        %     ) of
        %         {ok, [{[{id, Id1}]}]} ->
        %             Id1;
        %         Error ->
        %             Error
        %     end
        % ),
        % Friend_id = proplists:get_value(friend_id, Params,
        %     case empdb_dao_pers:get(Con,
        %         [   {isdeleted, false},
        %             {nick, proplists:get_value(friend_nick, Params, [])},
        %             {limit, 1}
        %         ], [id]
        %     ) of
        %         {ok, {[{id, Id2}]}} ->
        %             Id2;
        %         Error2 ->
        %             Error2
        %     end
        % ),
        empdb_dao_friend:delete(Con, Params)
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

get_emotion(Params, Fields)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_emotion(Con, [{isdeleted, false}|Params], Fields)
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

get_pgroup(Params, Fields)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_pgroup(Con, [{isdeleted, false}|Params], Fields)
    end).

update_pgroup(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:update_pgroup(Con, Params)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Чиновничий статус пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_ostatus(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_ostatus(Con, [{isdeleted, false}|Params])
    end).

get_ostatus(Params, Fields)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_ostatus(Con, [{isdeleted, false}|Params], Fields)
    end).

update_ostatus(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:update_ostatus(Con, Params)
    end).

    
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Семейное положение пользователя
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_mstatus(Params)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_mstatus(Con, [{isdeleted, false}|Params])
    end).

get_mstatus(Params, Fields)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_mstatus(Con, [{isdeleted, false}|Params], Fields)
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

get_pstatus(Params, Fields)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_pstatus(Con, [{isdeleted, false}|Params], Fields)
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

get_authority(Params, Fields)->
    empdb_dao:with_connection(emp, fun(Con)->
        empdb_dao_pers:get_authority(Con, [{isdeleted, false}|Params], Fields)
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

