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
        case empdb_dao_pers:get(Con, [
            {'or', [
                {id,      proplists:get_value(pers_id, Params, null)},
                {nick,    proplists:get_value(pers_nick, Params, null)}
            ]},
            {fields, [
                id,
                citizen_room_id,
                citizen_room_head,
                live_room_id,
                live_room_head
            ]},
            {limit, 1}
        ]) of
            {ok, [{Perspl}]} ->
                Id = proplists:get_value(id, Perspl),
                create__(Con, [
                    {isoffer,         false},
                    {pers_id, proplists:get_value(id, Perspl)},
                    {ss_pers_citizen_room_id,
                        proplists:get_value(citizen_room_id, Perspl)},
                    {ss_pers_citizen_room_head,
                        proplists:get_value(citizen_room_head, Perspl)},
                    {ss_pers_live_room_id,
                        proplists:get_value(live_room_id, Perspl)},
                    {ss_pers_live_room_head,
                        proplists:get_value(live_room_head, Perspl)}
                    |proplists:delete(pers_id, proplists:delete(pers_nick, Params))
                ]);
            {ok, []} ->
                create__(Con, [
                    {isoffer,                   true},
                    {pers_id,                   null},
                    {ss_pers_citizen_room_id,   null},
                    {ss_pers_citizen_room_head, null},
                    {ss_pers_live_room_id,      null},
                    {ss_pers_live_room_head,    null}
                    |proplists:delete(pers_id, proplists:delete(pers_nick, Params))
                ]);
                %{error, pers_does_not_exist};
            Elsepers ->
                Elsepers
        end
    end).



create__(Con, Params)->
    case empdb_dao_pers:get(Con, [
        {'or', [
            {id,      proplists:get_value(owner_id, Params, null)},
            {nick,    proplists:get_value(owner_nick, Params, null)}
        ]},
        {fields, [
            id,
            citizen_room_id,
            citizen_room_head,
            live_room_id,
            live_room_head
        ]},
        {limit, 1}
    ]) of
        {ok, [{Ownerpl}]} ->
            empdb_dao_claim:create(Con, [
                {owner_id, proplists:get_value(id, Ownerpl)},
                {ss_owner_citizen_room_id,
                    proplists:get_value(citizen_room_id, Ownerpl)},
                {ss_owner_citizen_room_head,
                    proplists:get_value(citizen_room_head, Ownerpl)},
                {ss_owner_live_room_id,
                    proplists:get_value(live_room_id, Ownerpl)},
                {ss_owner_live_room_head,
                    proplists:get_value(live_room_head, Ownerpl)},
                {claimtype_alias, open}
                |proplists:delete(owner_id, proplists:delete(owner_nick, Params))
            ]);
        {ok, []} ->
            {error, owner_does_not_exist};
        Elseowner ->
            Elseowner
    end.


update(Params)->
    empdb_dao:with_connection(fun(Con)->
        Id          = proplists:get_value(id, Params),
        Judge_id    = proplists:get_value(judge_id, Params),
        Judge_nick  = proplists:get_value(judge_nick, Params),
        case ((Judge_id == undefined) and (Judge_nick == undefined)) of
            true ->
                empdb_dao_claim:update(Con, Params);
            _ ->
                case empdb_dao_claim:update(Con, [
                    {filter, [
                        {id, Id},
                        {judge_id, null}
                    ]},
                    {values, [
                        {claimtype_alias, progress}
                        |Params
                    ]}
                ]) of
                    {ok, []} ->
                        {error, not_uniq_judge};
                    Else ->
                        Else
                end
        end
    end).

get(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_biz:nviewsupm(?MODULE, [Params]),
        empdb_dao_claim:get(Con, [{isdeleted, false}|Params])
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
                                citizen_room_id,
                                citizen_room_head,
                                citizen_room_fromdatetime,
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
                                citizen_room_id,
                                citizen_room_head,
                                citizen_room_fromdatetime,
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
                            {owner_citizen_room_id,
                                proplists:get_value(citizen_room_id, Ownerpl)
                            },
                            {owner_citizen_room_head,
                                proplists:get_value(citizen_room_head, Ownerpl)
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
                            },
                            {pers_citizen_room_id,
                                proplists:get_value(citizen_room_id, Perspl)
                            },
                            {pers_citizen_room_head,
                                proplists:get_value(citizen_room_head, Perspl)
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
    empdb_biz:nviewsupm(?MODULE, [Params]),
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
