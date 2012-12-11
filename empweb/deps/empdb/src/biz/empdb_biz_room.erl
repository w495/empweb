%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_room).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
    get/1,
    get/2,
    count/1,
    join/1,
    create/1,
    add_topic/1,
    delete_topic/1,
    update/1,
    delete/1
]).



-define(CREATE_ROOM_PRICE, 1.0).


%%
%% API Functions
%%


create(Params)->
    empdb_dao:with_transaction(fun(Con)->
        {ok, [{Mbownerpl}]} =
            empdb_dao_pers:get(Con, [
                {'or', [
                    {id,    proplists:get_value(owner_id,   Params)},
                    {nick,  proplists:get_value(owner_nick, Params)}
                ]},
                {fields, [
                    id,
                    nick,
                    money
                ]},
                {limit, 1}
            ]),
        %%
        %% Здесь надо вводить, дополнительное ограничение на уровне базы.
        %% Это делать не хочется, так как
        %%  * room --- это doc
        %%  * ввести ограничение:
        %%  * * введение нового поля с дублированием смысла head;
        %%  * * описание ограничения constrait конкретно для room;
        %%  * * в doc ввести поле isroom и сделать constrait на head и isroom;
        %%  * * => все это немного бредово.
        %%
        %% WARNING: В текущей реализации, мы действуем не очень эффективно.
        %%
        Head = proplists:get_value(head, Params),
        {ok, Mbroomobjs} =
            empdb_dao_room:get(Con, [
                {head, Head},
                {fields, [
                    id
                ]},
                {limit, 1}
            ]),
        Price = ?CREATE_ROOM_PRICE,
        Money = proplists:get_value(money, Mbownerpl),
        case {Price =< Money, Mbroomobjs} of
            {true, []} ->
                case empdb_dao_room:create(Con, Params) of
                    {ok, [{Respl}]} ->
                        {ok, _} = empdb_dao_pay:create(Con, [
                            {pers_id,           proplists:get_value(owner_id,   Params)},
                            {paytype_alias,     room_out},
                            {isincome,          false},
                            {price,             Price}
                        ]),
                        Newmoney = Money - Price,
                        empdb_dao_pers:update(Con,[
                            {id,    proplists:get_value(id,   Mbownerpl)},
                            {money, {decr, Price}}
                        ]),
                        {ok, [
                            {[
                                {money, Newmoney},
                                {price, Price}
                                |Respl
                            ]}
                        ]};
                    Else ->
                        Else
                end;
            {true, _} ->
                Sugs = suggest_head(Con, Head, [{owner, Mbownerpl}]),
                {error,{not_unique_head,Sugs}};
            {false, _ }->
                {error, {not_enough_money, {[
                    {money, Money},
                    {price, Price}
                ]}}};
            Error ->
                Error
        end
    end).


suggest_head(Con, Orghead, Opts)->
    Owner = proplists:get_value(owner, Opts),
    Owner_nick = proplists:get_value(nick, Owner),
    lists:sort(
        fun(X, Y) ->
            erlang:byte_size(X) < erlang:byte_size(Y)
        end,
        lists:sort(
            lists:filter(
                fun(Head)->
                    case empdb_dao_room:get(Con, [{head, Head}]) of
                        {ok, []} ->
                            true;
                        _ ->
                            false
                    end
                end,
                empdb_suggest:string(Orghead, [
                    {prewords, [
                        Owner_nick,
                        <<"room">>,
                        <<"country">>,
                        <<"place">>
                    ]},
                    {postwords, [
                        Owner_nick,
                        <<"room">>,
                        <<"country">>,
                        <<"place">>
                    ]},
                    {stopwords, [
                        Owner_nick,
                        <<"room">>,
                        <<"country">>,
                        <<"place">>
                    ]}
                ])
            )
        )
    ).

update(Params)->
    empdb_dao:with_transaction(fun(Con)->

%     Id = proplists:get_value(id, Params,
%         proplists:get_value(id,
%             proplists:get_value(filter, Params, [])
%         )
%     ),
%
%     Owner_id =  proplists:get_value(owner_id, Params,
%         proplists:get_value(owner_id,
%             proplists:get_value(filter, Params, [])
%         )
%     ),
%
%     Owner_nick =  proplists:get_value(owner_nick, Params,
%         proplists:get_value(owner_nick,
%             proplists:get_value(filter, Params, [])
%         )
%     ),
%
%     case proplists:get_value(treas, Params,
%         proplists:get_value(treas,
%             proplists:get_value(values, Params, [])
%         )
%     ) of
%         undefined ->
%             empdb_dao_room:update(Con, Params)
%         Treas ->
%
%
%
%     {ok, [{Mbownerpl}]} =
%         empdb_dao_pers:get(Con, [
%             {'or', [
%                 {id,    Owner_id},
%                 {nick,  Owner_nick}
%             ]},
%             {fields, [
%                 id,
%                 money
%             ]},
%             {limit, 1}
%         ]),
%

        empdb_dao_room:update(Con, Params)
    end).


join(Params)->
    empdb_dao:with_transaction(fun(Con)->
        Pers_id = proplists:get_value(pers_id, Params),
        Room_id = proplists:get_value(id, Params),
        {ok, [{[{ulimit, Ulimit}]}]} = empdb_dao_room:get(Con, [
            {filter, [
                {id, Room_id}
            ]},
            {fields, [ulimit]},
            {limit, 1}
        ]),
        {ok, [{[{count, Count}]}]} = empdb_dao_pers:count(Con, [
            {filter, [
                {live_room_id, Room_id}
            ]}
        ]),
        case ((Count + 1) =< (Ulimit)) of
            false ->
                {error, {user_overflow, {[
                    {count, Count},
                    {ulimit, Ulimit}
                ]}}};
            true ->
                {ok, _} = empdb_dao_pers:update(Con, [
                    {filter, [
                        {id, Pers_id}
                    ]},
                    {values, [
                        {live_room_id, Room_id}
                    ]}
                ]),
                empdb_dao_pers:get(Con, [
                    {filter, [
                        {live_room_id, Room_id}
                    ]},
                    {fields, [id]}
                ])
        end
    end).


delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_room:update(Con, [{isdeleted, true}|Params])
    end).

add_topic(Params)->
    empdb_dao:with_transaction(fun(Con)->
        case  empdb_dao_room:add_room_topic(Con, Params) of
            {ok, Res} ->
                empdb_dao_room:update_topic(Con, [
                    {id, proplists:get_value(topic_id, Params)},
                    {nchildtargets, {incr, 1}}
                ]),
                {ok, Res};
            Error ->
                Error
        end
    end).

delete_topic(Params)->
    empdb_dao:with_transaction(fun(Con)->
        case empdb_dao_room:delete_room_topic(Con, Params) of
            {ok, Res} ->
                empdb_dao_room:update_topic(Con, [
                    {id, proplists:get_value(topic_id, Params)},
                    {nchildtargets, {decr, 1}}
                ]),
                {ok, Res};
            Error ->
                Error
        end
    end).

count(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_room:count(Con, [
            {isdeleted, false}
            |Params
        ])
    end).

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        get_adds(Con, {
            empdb_dao_room:get(Con, [
                {order, {desc, doc.created}},
                {isdeleted, false}
                |Params
            ]),
            proplists:get_value(id, Params)
        })
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        get_adds(Con,{
            empdb_dao_room:get(Con, [
                {order, {desc, doc.created}},
                {isdeleted, false}
                |Params
            ], Fileds),
            proplists:get_value(id, Params)
        })
    end).

get_adds(Con, {Res, undefined}) ->
    Res;

get_adds(Con,{{ok, Rooms}, Id}) ->
    {ok,
        lists:map(fun({Roompl})->
            case empdb_dao_room:get_room_topic(Con, [
                {isdeleted, false},
                {room_id, Id}
            ]) of
                {ok, Topiclist} ->
                    {[{topic_list, Topiclist}|Roompl]};
                Error ->
                    {Roompl}
            end
        end, Rooms)
    };

get_adds(Con, {Err, _}) ->
    Err.


is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_room:is_owner(Con, Uid, Oid)
    end).

%%
%% Local Functions
%%

