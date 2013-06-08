%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_dao_claim).
-behaviour(empdb_dao).

%%
%% Include files
%%


%%
%% Exported Functions
%%
-export([
    table/1,
    table/0,
    create/2,
    update/2,
    count_comments/2,
    get_adds/2,
    get/2,
    get/3
]).

%%
%% API Functions
%%



%%
%% @doc Возвращает список обязательных полей таблицы для создания
%%
table({fields, insert, required})-> [];

%%
%% @doc Возвращает список полей таблицы для выборки
%%
table({fields, select})->
    table({fields, all});

%%
%% @doc Возвращает список полей таблицы для обновления
%%
table({fields, update})->
    table({fields, all}) -- [id];

%%
%% @doc Возвращает список полей таблицы для создания
%%
table({fields, insert})->
    table({fields, all}) -- [id];

%%
%% @doc Возвращает полный список полей таблицы
%%
table({fields, all})->
    [
        isoffer,
        doc_id,
        pers_id,
        pers_nick,
        judge_id,
        judge_nick,
        claimtype_id,
        claimtype_alias,
        ss_owner_citizen_room_id,
        ss_owner_citizen_room_head,
        ss_owner_live_room_id,
        ss_owner_live_room_head,
        ss_pers_citizen_room_id,
        ss_pers_citizen_room_head,
        ss_pers_live_room_id,
        ss_pers_live_room_head
    ];

%%
%% @doc Возвращает полный список полей таблицы
%%
table(fields)->
    table({fields, all});

%%
%% @doc Возвращает имя таблицы
%%
table(name)->
    claim.

table()->
    table(name).

get(Con, What) ->
    Fields =
        case proplists:get_value(fields, What, []) of
            [] ->
                lists:append(
                    [
                        empdb_dao_claim:table({fields, select}),
                        empdb_dao_doc:table({fields, select}),
                        [
                            owner_citizen_room_id,
                            owner_citizen_room_head,
                            pers_citizen_room_id,
                            judge_citizen_room_head,
                            judge_citizen_room_id,
                            judge_citizen_room_head,
                            owner_authority_id,
                            owner_authority_alias,
                            pers_authority_id,
                            pers_authority_alias,
                            judge_authority_id,
                            judge_authority_alias
                        ]

                    ]
                );
            F ->
                F
        end,


    empdb_dao:get([
        {empdb_dao_doc,     id},
        {empdb_dao_claim,   doc_id},
        {{empdb_dao_pers, owner },   {id,   {doc,    owner_id}}},
        {{empdb_dao_pers, pers  },   {left, {id, {claim,  pers_id}}}},
        {{empdb_dao_pers, judge },   {left, {id, {claim,  judge_id}}}}
    ],Con,[
        {fields, Fields}
        |proplists:delete(fields, What)
    ]).

get(Con, What, Afields)->
     Fields =
        case Afields of
            [] ->
                lists:append(
                    [
                        empdb_dao_claim:table({fields, select}),
                        empdb_dao_doc:table({fields, select}),
                        [
                            owner_citizen_room_id,
                            owner_citizen_room_head,
                            pers_citizen_room_id,
                            pers_citizen_room_head,
                            judge_citizen_room_id,
                            judge_citizen_room_head,
                            owner_authority_id,
                            owner_authority_alias,
                            pers_authority_id,
                            pers_authority_alias,
                            judge_authority_id,
                            judge_authority_alias
                        ]

                    ]
                );
            F ->
                F
        end,
    empdb_dao:get([
        {empdb_dao_doc,     id},
        {empdb_dao_claim,   doc_id},
        {{empdb_dao_pers, owner },   {id,   {doc,    owner_id}}},
        {{empdb_dao_pers, pers  },   {id,   {claim,  pers_id}}},
        {{empdb_dao_pers, judge },   {left, {id, {claim,  judge_id}}}}
    ],Con,[
        {fields, Fields}
        |proplists:delete(fields, What)
    ]).

create(Con, Proplist)->
    empdb_dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    empdb_dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    empdb_dao_doc:is_owner(Con, Owner_id, Obj_id).

count_comments(Con, Params)->
    empdb_dao:eqret(Con,
        " select "
            " count(doc_comment.id) "
        " from "
            " doc as doc_comment "
        " where "
            "       doc_comment.doctype_alias  = 'comment' "
            " and   doc_comment.isdeleted      = false "
            " and   doc_comment.parent_id      = $id ",
        Params
    ).


get_adds(Con, Getresult) ->
    case Getresult of
        {ok, List} ->
            {ok, lists:map(fun({Itempl})->
                case proplists:get_value(id, Itempl) of
                    undefined ->
                        {Itempl};
                    Id ->
                        {ok, Comments}     = ?MODULE:count_comments(Con, [{id, Id}]),
                        Ncommentspl = lists:foldl(fun({Commentspl}, Acc)->
                            [{ncomments, proplists:get_value(count, Commentspl)}|Acc]
                        end, [], Comments),
                        {lists:append([Ncommentspl, Itempl])}
                end
            end, List)};
        {Eclass, Error} ->
            {Eclass, Error}
    end.
%%
%% Local Functions
%%

