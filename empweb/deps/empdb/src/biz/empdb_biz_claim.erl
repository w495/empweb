%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_claim).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").


%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Блоги
%%
-export([
    get/1,
    get/2,
    create/1,
    update/1
]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          ЗНАЧИМЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Блоги
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Params)->
    empdb_dao:with_connection(fun(Con)->
        case proplists:get_value(pers_nick, Params) of
            undefined ->
                empdb_dao_claim:create(Con, Params);
            Nick ->
                case empdb_dao_pers:get(Con, [
                    {nick, Nick},
                    {fields, [id]},
                    {limit, 1}
                ]) of
                    {ok, [{[{id, Id}]}]} ->
                        empdb_dao_claim:create(Con, [
                            {pers_id, Id}
                            |proplists:delete(pers_nick, Params)
                        ]);
                    Else ->
                        {error, pers_nick_do_not_exists}
                end
        end
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_claim:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_connection(fun(Con)->
        get_adds(
            Con,
            empdb_dao_claim:get(Con, [{isdeleted, false}|Params]),
            Params
        )
    end).

get_adds(Con, {ok, Res}, Params) ->
    Fields = proplists:get_value(fields, Params, []),
    % case lists:member(Option, Fields) or (Fields =:= []) of
    {ok,
        lists:map(
            fun({Itempl})->
                {ok, [{Ownerpl}]} =
                    empdb_dao_pers:get(
                        Con,
                        [
                            {'or', [
                                {id,    proplists:get_value(owner_id,   Itempl)},
                                {nick,  proplists:get_value(owner_nick, Itempl)}
                            ]},
                            {fields, [
                                authority_id,
                                authority_alias,
                                live_room_id,
                                live_room_head
                            ]}
                        ]
                    ),
                {ok, [{Perspl}]} =
                    empdb_dao_pers:get(
                        Con,
                        [
                            {'or', [
                                {id,    proplists:get_value(pers_id,   Itempl)},
                                {nick,  proplists:get_value(pers_nick, Itempl)}
                            ]},
                            {fields, [
                                authority_id,
                                authority_alias,
                                live_room_id,
                                live_room_head
                            ]}
                        ]
                    ),
                {
                    lists:foldl(
                        fun({Key, Value}, Acc)->
                            case lists:member(Key, Fields) or (Fields =:= []) of
                                true ->
                                    [{Key, Value}|Acc];
                                false ->
                                    Acc
                            end
                        end,
                        Itempl,
                        [
                            {owner_authority_id,
                                proplists:get_value(authority_id, Ownerpl)
                            },
                            {owner_authority_alias,
                                proplists:get_value(authority_alias, Ownerpl)
                            },
                            {owner_live_room_id,
                                proplists:get_value(live_room_id, Ownerpl)
                            },
                            {owner_live_room_head,
                                proplists:get_value(live_room_head, Ownerpl)
                            },
                            {pers_authority_id,
                                proplists:get_value(authority_id, Perspl)
                            },
                            {pers_authority_alias,
                                proplists:get_value(authority_alias, Perspl)
                            },
                            {pers_live_room_id,
                                proplists:get_value(live_room_id, Perspl)
                            },
                            {pers_live_room_head,
                                proplists:get_value(live_room_head, Perspl)
                            }
                        ]
                    )
                }
            end,
            Res
        )
    };

get_adds(_Con, Else, Params) ->
    Else.

get(Params, Fileds)->
    empdb_dao:with_connection(fun(Con)->
%         get_adds(
%             Con,
            empdb_dao_claim:get(Con, [{isdeleted, false}|Params], Fileds)
%         )
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_claim:is_owner(Con, Uid, Oid)
    end).
