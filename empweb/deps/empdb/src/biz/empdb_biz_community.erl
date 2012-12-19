%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_community).

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
    create/1,
    update/1,
    delete/1
]).



-define(CREATE_COMMUNITY_PRICE, 1.0).


%%
%% API Functions
%%

create(Params)->
    Owner_id = proplists:get_value(owner_id, Params, []),
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
        %%  * community --- это doc
        %%  * ввести ограничение:
        %%  * * введение нового поля с дублированием смысла head;
        %%  * * описание ограничения constrait конкретно для community;
        %%  * * в doc ввести поле iscommunity и сделать constrait на head и iscommunity;
        %%  * * => все это немного бредово.
        %%
        %% WARNING: В текущей реализации, мы действуем не очень эффективно.
        %%
        Head = proplists:get_value(head, Params),
        {ok, Mbcommobjs} = empdb_dao_community:get(Con, [
            {'or', [
                {owner_id, Owner_id},
                {head, Head}
            ]},
            {fields, [
                head,
                owner_id
            ]},
            {limit, 1},
            {isdeleted, false}
        ]),
        Price = ?CREATE_COMMUNITY_PRICE,
        Money = proplists:get_value(money, Mbownerpl),
         case {Price =< Money, Mbcommobjs} of
            {true, []} ->
                case empdb_dao_community:create(Con, [{nmembs, 1}|Params]) of
                    {ok, [{Respl}]} ->
                        {ok, _} = empdb_dao_pay:create(Con, [
                            {pers_id,           proplists:get_value(owner_id,   Params)},
                            {paytype_alias,     community_out},
                            {isincome,          false},
                            {price,             Price}
                        ]),
                        Newmoney = Money - Price,
                        {ok, _} = empdb_dao_pers:update(Con, [
                            {filter, [
                                {id, Owner_id}
                            ]},
                            {values, [
                                {money, {decr, Price}},
                                {own_community_id, proplists:get_value(id, Respl)},
                                {live_community_id, proplists:get_value(id, Respl)},
                                {live_community_approved, true}
                            ]}
                        ]),
                        {ok, [
                            {[
                                {money, Newmoney},
                                {price, Price}
                                |Respl
                            ]}
                        ]};
                    Error ->
                        Error
                end;
            {true, [{Mbcommpl}|_]} ->
                case {
                    proplists:get_value(head, Mbcommpl),
                    proplists:get_value(owner_id, Mbcommpl)
                } of
                    {_, Owner_id } ->
                        {error,{not_unique_owner,Owner_id}};
                    {Head, _} ->
                        Sugs = suggest_head(Con, Head, [{owner, Mbownerpl}]),
                        {error,{not_unique_head,Sugs}}
                end;
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
                    case empdb_dao_community:get(Con, [{head, Head}]) of
                        {ok, []} ->
                            true;
                        _ ->
                            false
                    end
                end,
                empdb_suggest:string(Orghead, [
                    {prewords, [
                        Owner_nick,
                        <<"community">>,
                        <<"gang">>,
                        <<"crowd">>,
                        <<"horde">>,
                        <<"mob">>,
                        <<"band">>,
                        <<"fraternity">>,
                        <<"fellowship">>,
                        <<"society">>,
                        <<"brotherhood">>
                    ]},
                    {postwords, [
                        Owner_nick,
                        <<"community">>,
                        <<"gang">>,
                        <<"crowd">>,
                        <<"horde">>,
                        <<"mob">>,
                        <<"band">>,
                        <<"fraternity">>,
                        <<"fellowship">>,
                        <<"society">>,
                        <<"brotherhood">>
                    ]},
                    {stopwords, [
                        Owner_nick,
                        <<"community">>,
                        <<"gang">>,
                        <<"crowd">>,
                        <<"horde">>,
                        <<"mob">>,
                        <<"band">>,
                        <<"fraternity">>,
                        <<"fellowship">>,
                        <<"society">>,
                        <<"brotherhood">>
                    ]}
                ])
            )
        )
    ).
update(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_community:update(Con, Params)
    end).

delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        case empdb_dao_community:update(Con, [
            {filter, [{isdeleted, false} |Params]},
            {values, [{isdeleted, true}]}
        ]) of
            {ok, List}->
                lists:map(
                    fun({Communitypl})->
                        {ok, _} = empdb_dao_pers:update(Con, [
                            {filter, [
                                {live_community_id,
                                    proplists:get_value(id, Communitypl)}
                            ]},
                            {values, [
                                {live_community_id,         null},
                                {live_community_approved,   null}
                            ]}
                        ])
                    end,
                    List
                ),
                {ok, List};
            Error ->
                Error
        end
    end).



add_topic(Params)->
    empdb_dao:with_transaction(fun(Con)->
        case  empdb_dao_community:add_community_topic(Con, Params) of
            {ok, Res} ->
                empdb_dao_community:update_topic(Con, [
                    {id, proplists:get_value(topic_id, Params)},
                    {nchildtargets, {incr, 1}},
                    {ncommunitytargets, {incr, 1}}
                ]),
                {ok, Res};
            Error ->
                Error
        end
    end).

delete_topic(Params)->
    empdb_dao:with_transaction(fun(Con)->
        case empdb_dao_community:delete_community_topic(Con, Params) of
            {ok, Res} ->
                empdb_dao_community:update_topic(Con, [
                    {id, proplists:get_value(topic_id, Params)},
                    {nchildtargets, {decr, 1}},
                    {ncommunitytargets, {decr, 1}}
                ]),
                {ok, Res};
            Error ->
                Error
        end
    end).

count(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_community:count(Con, [
            {isdeleted, false}
            |Params
        ])
    end).


%
% get(Params)->
%     empdb_dao:with_transaction(fun(Con)->
%         empdb_dao_community:get(Con, [
%             {isdeleted, false}
%             |Params
%         ] ++ [
%             {order, {asc, head}}
%         ])
%     end).
%
% get(Params, Fileds)->
%     empdb_dao:with_transaction(fun(Con)->
%         empdb_dao_community:get(Con, [
%                 {isdeleted, false}
%                 |Params
%             ] ++ [
%                 {order, {asc, head}}
%             ],
%             Fileds
%         )
%     end).
%

get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        get_adds(Con, {
            empdb_dao_community:get(Con, [
                {isdeleted, false}
                |Params
            ] ++ [
                {order, {asc, head}}
            ]),
            proplists:get_value(id, Params)
        })
    end).

get(Params, Fileds)->
    empdb_dao:with_transaction(fun(Con)->
        get_adds(Con,{
            empdb_dao_community:get(Con, [
                {isdeleted, false}
                |Params
            ] ++ [
                {order, {asc, head}}
            ], Fileds),
            proplists:get_value(id, Params)
        })
    end).

get_adds(Con, {Res, undefined}) ->
    Res;

get_adds(Con,{{ok, Rooms}, Id}) ->
    {ok,
        lists:map(fun({Roompl})->
            case empdb_dao_community:get_community_topic(Con, [
                {isdeleted, false},
                {community_id, Id}
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
        empdb_dao_community:is_owner(Con, Uid, Oid)
    end).


%%
%% Local Functions
%%

