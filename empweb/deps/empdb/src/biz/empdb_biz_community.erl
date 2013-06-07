%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_community).

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").

%%
%% Exported Functions
%%
-export([
    get/1,
    get/2,
    get_blogs/1,
    get_posts/1,
    get_photos/1,
    add_topic/1,
    delete_topic/1,
    count/1,
    create/1,
    update/1,
    delete/1
]).


% 
% -define\(CREATE_COMMUNITY_PRICE, 1.0).
%

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
                    money,
                    authority_id,
                    authority_level
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
        {ok, Mbcommunityobjs} =
            empdb_dao_community:get(Con, [
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

        {ok,[{Servicepl}]} =
            empdb_dao_service:get(
                Con,
                [
                    {alias, create_community_price},
                    {fields, [price]},
                    {limit, 1}
                ]
            ),

        Price = proplists:get_value(price, Servicepl),
        Money = proplists:get_value(money, Mbownerpl),
        {ok, [{Readgteauthoritypl}]} =
            empdb_dao_authority:get(Con, [
                {'or',[
                    {id, proplists:get_value(read_gte_authority_id, Params)},
                    {alias,
                        proplists:get_value(read_gte_authority_alias, Params, noob)}
                ]},
                {limit, 1}
            ]),
        {ok, [{Candsgteauthoritypl}]} =
            empdb_dao_authority:get(Con, [
                {'or',[
                    {id, proplists:get_value(cands_gte_authority_id, Params)},
                    {alias,
                        proplists:get_value(cands_gte_authority_alias, Params, noob)}
                ]},
                {limit, 1}
            ]),
        Authority_level =
            proplists:get_value(authority_level, Mbownerpl),
        Readgteauthority_level =
            proplists:get_value(level, Readgteauthoritypl, 0),
        Candsgteauthority_level =
            proplists:get_value(level, Candsgteauthoritypl, 0),
        case {
            Price =< Money,
            Mbcommunityobjs,
            (Authority_level >= Readgteauthority_level)
                and
            (Authority_level >= Candsgteauthority_level)
        } of
            {true, [], true} ->
                case empdb_dao_community:create(Con, Params) of
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
                                {own_community_id,      proplists:get_value(id, Respl)},
                                {own_community_head,    Head},
                                {live_community_id,     proplists:get_value(id, Respl)},
                                {live_community_head,   Head},
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
            {true, Mbcommunityobjs, true} ->
                case lists:foldl(
                    fun
                        (_, {error,{not_unique_owner,Owner_id}})->
                            {error,{not_unique_owner,Owner_id}};
                        ({Mbcommunityplownerid}, undefined)->
                            case proplists:get_value(owner_id, Mbcommunityplownerid) of
                                Owner_id ->
                                    {error,{not_unique_owner,Owner_id}};
                                _ ->
                                    undefined
                            end
                    end,
                    undefined,
                    Mbcommunityobjs
                ) of
                    undefined ->
                        Sugs = suggest_head(Con, Head, [{owner, Mbownerpl}]),
                        {error,{not_unique_head,Sugs}};
                    Not_unique_owner ->
                        Not_unique_owner
                end;
            {false, _ ,true}->
                {error, {not_enough_money, {[
                    {money, Money},
                    {price, Price}
                ]}}};
            {_, _, false}->
                {error, {not_enough_level, {[
                    {cands_gte_authority_level,
                        Candsgteauthority_level},
                    {read_gte_authority_level,
                        Readgteauthority_level},
                    {authority_level,
                        Authority_level}
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
                    money,
                    authority_id,
                    authority_level
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
        {ok, Mbcommunityobjs} =
            empdb_dao_community:get(Con, [
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
        {ok,[{Servicepl}]} =
            empdb_dao_service:get(
                Con,
                [
                    {alias, create_community_price},
                    {fields, [price]},
                    {limit, 1}
                ]
            ),

        Price = proplists:get_value(price, Servicepl),
        
        Money = proplists:get_value(money, Mbownerpl),
        {ok, [{Readgteauthoritypl}]} =
            empdb_dao_authority:get(Con, [
                {'or',[
                    {id, proplists:get_value(read_gte_authority_id, Params)},
                    {alias,
                        proplists:get_value(read_gte_authority_alias, Params, noob)}
                ]},
                {limit, 1}
            ]),
        {ok, [{Candsgteauthoritypl}]} =
            empdb_dao_authority:get(Con, [
                {'or',[
                    {id, proplists:get_value(cands_gte_authority_id, Params)},
                    {alias,
                        proplists:get_value(cands_gte_authority_alias, Params, noob)}
                ]},
                {limit, 1}
            ]),
        Authority_level =
            proplists:get_value(authority_level, Mbownerpl),
        Readgteauthority_level =
            proplists:get_value(level, Readgteauthoritypl),
        Candsgteauthority_level =
            proplists:get_value(level, Candsgteauthoritypl),

        case 
            ((Authority_level >= Readgteauthority_level)
                and
            (Authority_level >= Candsgteauthority_level))
        of
            false ->
                {error, {not_enough_level, {[
                    {cands_gte_authority_level,
                        Candsgteauthority_level},
                    {read_gte_authority_level,
                        Readgteauthority_level},
                    {authority_level,
                        Authority_level}
                ]}}};
            true ->
                empdb_dao_community:update(Con, Params)
        end
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



get(Params)->
    empdb_biz:nviewsupm(?MODULE, [Params]),
    empdb_dao:with_transaction(fun(Con)->
        get_con(Con, Params)
    end).

get(Params, Fields)->
    empdb_biz:nviewsupm(?MODULE, [Params]),
    empdb_dao:with_transaction(fun(Con)->
        get_con(Con, Params, Fields)
    end).

get_con(Con, Params)->
    get_adds(Con,
        empdb_dao_community:get(Con, [
            {isdeleted, false}
            |Params
        ] ++ [
            {order, {asc, head}}
        ]),
        Params
    ).

get_con(Con, Params, Fields)->
    get_adds(Con,
        empdb_dao_community:get(Con, [
            {isdeleted, false}
            |Params
        ] ++ [
            {order, {asc, head}}
        ], Fields),
        [{fields, Fields}| Params]
    ).




get_blogs(Params) ->
    Isweek = proplists:get_value(isweek, Params, false),
    What =
        lists:keyreplace(
            id,
            1 ,
            proplists:delete(isweek, Params),
            {'pers.live_community_id', proplists:get_value(id, Params, null)}
        ),
    empdb_dao:with_transaction(fun(Con)->
        Truefields = proplists:get_value(fields,What,[]),
        Fields =
            case Truefields of
                [] ->
                    lists:append([
                        lists:foldl(
                            fun (X, A)->
                                    [
                                        erlang:list_to_atom(
                                            erlang:atom_to_list(blogs)
                                            ++ "." ++
                                            erlang:atom_to_list(X)
                                        )
                                        |A
                                    ]
                            end,
                            [],
                            empdb_dao_photo:table({fields, select})
                        ),
                        lists:foldl(
                            fun (X, A)->
                                    [
                                        erlang:list_to_atom(
                                            erlang:atom_to_list(doc)
                                            ++ "." ++
                                            erlang:atom_to_list(X)
                                        )
                                        |A
                                    ]
                            end,
                            [
                                case Isweek of
                                    false  ->'doc.created';
                                    true ->'doc.nvotes'end
                            ],
                            empdb_dao_doc:table({fields, select})
                        )
                    ]);
                _ ->
                    Truefields
            end,
        What_ = proplists:delete(fields, What),
        empdb_dao:get([
            {empdb_dao_doc,  id},
            {empdb_dao_blog,  doc_id},
            {empdb_dao_pers,  {left, {id, {doc, owner_id}}}}
            |
            case Isweek of
                false  ->
                    [];
                true  ->
                    [{{empdb_dao_vote,  vote},  {left, {doc_id,   {doc, id}}}}]
            end
        ],Con,[
            {distinct, ['doc.id']},
            {fields, Fields},
            {'doc.isdeleted',false},
            {'doc.isrepost',false},
            {'doc.isrepostcont',false}
            |
            case Isweek of
                false  ->
                    [
                        {order, {desc,'doc.created'}}
                        | What_
                    ];
                true  ->
                    [
                        {order, {desc,'doc.nvotes'}},
                        {'vote.created',
                            {gt, empdb_convert:now_minus_week()}
                        }
                        | What_
                    ]
            end
        ])
    end).

get_posts(Params) ->
    Isweek = proplists:get_value(isweek, Params, false),
    What =
        lists:keyreplace(
            id,
            1 ,
            proplists:delete(isweek, Params),
            {'pers.live_community_id', proplists:get_value(id, Params, null)}
        ),
    empdb_dao:with_transaction(fun(Con)->
        Truefields = proplists:get_value(fields,What,[]),
        Fields =
            case Truefields of
                [] ->
                    lists:append([
                        lists:foldl(
                            fun (X, A)->
                                    [
                                        erlang:list_to_atom(
                                            erlang:atom_to_list(post)
                                            ++ "." ++
                                            erlang:atom_to_list(X)
                                        )
                                        |A
                                    ]
                            end,
                            [],
                            empdb_dao_post:table({fields, select})
                        ),
                        lists:foldl(
                            fun (id, A)->
                                    A;
                                (X, A)->
                                    [
                                        erlang:list_to_atom(
                                            erlang:atom_to_list(doc)
                                            ++ "." ++
                                            erlang:atom_to_list(X)
                                        )
                                        |A
                                    ]
                            end,
                            [
                                case Isweek of
                                    false  ->'doc.created';
                                    true ->'doc.nvotes'end
                            ],
                            empdb_dao_doc:table({fields, select})
                        )
                    ]);
                _ ->
                    Truefields
            end,
        What_ = proplists:delete(fields, What),
        empdb_dao:get([
            {empdb_dao_doc,  id},
            {empdb_dao_post,  doc_id},
            {empdb_dao_pers,  {left, {id, {doc, owner_id}}}}
            |
            case Isweek of
                false  ->
                    [];
                true  ->
                    [{{empdb_dao_vote,  vote},  {left, {doc_id,   {doc, id}}}}]
            end
        ],Con,[
            {distinct, ['doc.id']},
            {fields, Fields},
            {'doc.isdeleted',false},
            {'doc.isrepost',false},
            {'doc.isrepostcont',false}
            |
            case Isweek of
                false  ->
                    [
                        {order, {desc,'doc.created'}}
                        | What_
                    ];
                true  ->
                    [
                        {order, {desc,'doc.nvotes'}},
                        {'vote.created',
                            {gt, empdb_convert:now_minus_week()}
                        }
                        | What_
                    ]
            end
        ])
    end).

get_photos(Params) ->
    Isweek = proplists:get_value(isweek, Params, false),
    What =
        lists:keyreplace(
            id,
            1 ,
            proplists:delete(isweek, Params),
            {'pers.live_community_id', proplists:get_value(id, Params, null)}
        ),
    empdb_dao:with_transaction(fun(Con)->
        Truefields = proplists:get_value(fields,What,[]),
        Fields =
            case Truefields of
                [] ->
                    lists:append([
                        lists:foldl(
                            fun (X, A)->
                                    [
                                        erlang:list_to_atom(
                                            erlang:atom_to_list(photo)
                                            ++ "." ++
                                            erlang:atom_to_list(X)
                                        )
                                        |A
                                    ]
                            end,
                            [],
                            empdb_dao_photo:table({fields, select})
                        ),
                        lists:foldl(
                            fun (id, A)->
                                    A;
                                (X, A)->
                                    [
                                        erlang:list_to_atom(
                                            erlang:atom_to_list(doc)
                                            ++ "." ++
                                            erlang:atom_to_list(X)
                                        )
                                        |A
                                    ]
                            end,
                            [
                                case Isweek of
                                    false  ->'doc.created';
                                    true ->'doc.nvotes'end
                            ],
                            empdb_dao_doc:table({fields, select})
                        )
                    ]);
                _ ->
                    Truefields
            end,
        What_ = proplists:delete(fields, What),
        case empdb_dao:get([
            {empdb_dao_doc,  id},
            {empdb_dao_photo, {doc_id, file_id}},
            {empdb_dao_file, id},
            {empdb_dao_fileinfo, file_id},
            {empdb_dao_pers,  {left, {id, {doc, owner_id}}}}
            |
            case Isweek of
                false  ->
                    [];
                true  ->
                    [{{empdb_dao_vote,  vote},  {left, {doc_id,   {doc, id}}}}]
            end
        ],Con,[
            {distinct, ['doc.id']},
            {fileinfotype_alias, download},
            {fields, [
                {as, {'fileinfo.path', fileinfopath}},
                {as, {'fileinfo.dir',  fileinfodir}}
                | proplists:delete(path, Fields)
            ]},
            {'doc.isdeleted',false},
            {'doc.isrepost',false},
            {'doc.isrepostcont',false}
            |
            case Isweek of
                false  ->
                    [
                        {order, {desc,'doc.created'}}
                        | What_
                    ];
                true  ->
                    [
                        {order, {desc,'doc.nvotes'}},
                        {'vote.created',
                            {gt, empdb_convert:now_minus_week()}
                        }
                        | What_
                    ]
            end
        ]) of
            {ok,Phobjs} ->
                {ok,
                    lists:map(fun({Phpl})->
                        case (lists:member('photo.path', Fields) or (Fields =:= [])) of
                            true ->
                                {[
                                    {path,
                                        <<  (proplists:get_value(fileinfodir, Phpl))/binary,
                                            (proplists:get_value(fileinfopath, Phpl))/binary
                                        >>
                                    }
                                    | proplists:delete(fileinfodir,
                                        proplists:delete(fileinfopath,
                                            proplists:delete(path, Phpl)))
                                ]};
                            _ ->
                                {proplists:delete(fileinfodir, proplists:delete(fileinfopath, Phpl))}
                        end
                    end, Phobjs)
                };
            Error ->
                Error
        end
    end).




get_adds(Con, {ok, Communitys}, Params) ->
    Fields = proplists:get_value(fields, Params, []),
    {ok,
        lists:map(
            fun({Communitypl})->
                {ok, Topiclist} =
                    empdb_dao_community:get_community_topic(Con, [
                        {isdeleted, false},
                        {community_id,proplists:get_value(id, Communitypl)}
                    ]),

                Backfilepath = filepath(Con, Communitypl, back_file_id),
                Wallfilepath = filepath(Con, Communitypl, wall_file_id),
                Flagfilepath = filepath(Con, Communitypl, flag_file_id),
                Armsfilepath = filepath(Con, Communitypl, arms_file_id),

                {
                    lists:foldl(
                        fun
                            ({Key, Value}, Acc)->
                                case lists:member(Key, Fields) or (Fields =:= []) of
                                    true ->
                                        [{Key, Value}|Acc];
                                    false ->
                                        Acc
                                end
                        end,
                        Communitypl,
                        [
                            {topic_list,        Topiclist},
                            {back_file_path,    Backfilepath},
                            {wall_file_path,    Wallfilepath},
                            {flag_file_path,    Flagfilepath},
                            {arms_file_path,    Armsfilepath}
                        ]
                    )
                }
            end,
            Communitys
        )
    };

get_adds(_con, Else, _params) ->
    Else.

filepath(Con, Communitypl, Idfield) ->
    case empdb_dao:get([
        {empdb_dao_file, id},
        {empdb_dao_fileinfo, file_id}
    ],Con,[
        {'file.id',
            proplists:get_value(Idfield, Communitypl)},
        {fileinfotype_alias,
            download},
        {limit, 1},
        {fields, [
            {as, {'fileinfo.path', fileinfopath}},
            {as, {'fileinfo.dir',  fileinfodir}}
        ]}
    ]) of
        {ok, []} ->
            null;
        {ok, [{List}]} ->
            <<  (proplists:get_value(fileinfodir, List))/binary,
                (proplists:get_value(fileinfopath, List))/binary
            >>
    end.




is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_community:is_owner(Con, Uid, Oid)
    end).


%%
%% Local Functions
%%

