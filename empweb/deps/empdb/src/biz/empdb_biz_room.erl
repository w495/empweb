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
    get_blogs/1,
    get_posts/1,
    get_photos/1,
    count/1,
    join/1,
    create/1,
    add_topic/1,
    delete_topic/1,
    update/1,
    delete/1
]).


%
% -define\(CREATE_ROOM_PRICE, 1.0).
%

%%
%% API Functions
%%


create(Params)->
    Room_authority_alias = inhabitant,
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
                    exper,
                    authority_id,
                    authority_alias,
                    authority_level
                ]},
                {limit, 1}
            ]),
        Owner_id = proplists:get_value(id,   Mbownerpl),
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
                {'or', [
                    {owner_id, Owner_id},
                    {head, Head}
                ]},
                {fields, [
                    id,
                    head,
                    owner_id
                ]}
            ]),

        io:format("~n~n~n Mbroomobjs  = ~p~n~n~n", [Mbroomobjs ]),

        {ok,[{Servicepl}]} =
            empdb_dao_service:get( Con, [
                    {alias, create_room_price},
                    {fields, [price]},
                    {limit, 1}
            ]),

        {ok, [{Authority}]} =
            empdb_dao_authority:get(Con, [
                {alias, Room_authority_alias},
                {limit, 1}
            ]),
            
        Price = proplists:get_value(price, Servicepl),
        Money = proplists:get_value(money, Mbownerpl),

        Room_authority_level =
            proplists:get_value(level, Authority),

        Pers_authority_level =
            proplists:get_value(authority_level, Mbownerpl),

        Pers_authority_alias =
            proplists:get_value(authority_alias, Mbownerpl),

        case {Money >= Price, Pers_authority_level >= Room_authority_level,  Mbroomobjs} of
            {true, true, []} ->
                case empdb_dao_room:create(Con, Params) of
                    {ok, [{Respl}]} ->
                        {ok, _} = empdb_dao_pay:create(Con, [
                            {pers_id,           Owner_id},
                            {paytype_alias,     room_out},
                            {isincome,          false},
                            {price,             Price}
                        ]),
                        Newmoney = Money - Price,
                        empdb_dao_pers:update(Con,[
                            {id,
                                proplists:get_value(id,   Mbownerpl)},
                            {own_room_id,
                                proplists:get_value(id, Respl)},
                            {own_room_head,
                                proplists:get_value(id, Respl)},
                            {citizen_room_id,
                                proplists:get_value(id, Respl)},
                            {citizen_room_head,
                                proplists:get_value(id, Respl)},
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
            {true, true, Mbroomobjs} ->
                case lists:foldl(
                    fun
                        (_, {error,{not_unique_owner,Owner_id}})->
                            {error,{not_unique_owner,Owner_id}};
                        ({Mbroomplownerid}, undefined)->
                            case proplists:get_value(owner_id, Mbroomplownerid) of
                                Owner_id ->
                                    {error,{not_unique_owner,Owner_id}};
                                _ ->
                                    undefined
                            end
                    end,
                    undefined,
                    Mbroomobjs
                ) of
                    undefined ->
                        Sugs = suggest_head(Con, Head, [{owner, Mbownerpl}]),
                        {error,{not_unique_head,Sugs}};
                    Not_unique_owner ->
                        Not_unique_owner
                end;
            {true, false,  _ }->
                {error, {not_enough_money, {[
                    {pers_authority_alias, Pers_authority_alias},
                    {pers_authority_level, Pers_authority_level},
                    {room_authority_alias, Room_authority_alias},
                    {room_authority_level, Room_authority_level}
                ]}}};
            {false, true,  _ }->
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
                    {nchildtargets, {incr, 1}},
                    {nroomtargets,  {incr, 1}}
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
                    {nchildtargets, {decr, 1}},
                    {nroomtargets,  {decr, 1}}
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




get_blogs(Params) ->
    Isweek = proplists:get_value(isweek, Params, false),
    What =
        lists:keyreplace(
            id,
            1 ,
            proplists:delete(isweek, Params),
            {'pers.citizen_room_id', proplists:get_value(id, Params, null)}
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
                                    false  ->
                                        doc.created;
                                    true ->
                                        doc.nvotes
                                end
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
            {distinct, [doc.id]},
            {fields, Fields},
            {doc.isdeleted,false},
            {doc.isrepost,false},
            {doc.isrepostcont,false}
            |
            case Isweek of
                false  ->
                    [
                        {order, {desc, doc.created}}
                        | What_
                    ];
                true  ->
                    [
                        {order, {desc, doc.nvotes}},
                        {vote.created,
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
            {'pers.citizen_room_id', proplists:get_value(id, Params, null)}
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
                                    false  ->
                                        doc.created;
                                    true ->
                                        doc.nvotes
                                end
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
            {distinct, [doc.id]},
            {fields, Fields},
            {doc.isdeleted,false},
            {doc.isrepost,false},
            {doc.isrepostcont,false}
            |
            case Isweek of
                false  ->
                    [
                        {order, {desc, doc.created}}
                        | What_
                    ];
                true  ->
                    [
                        {order, {desc, doc.nvotes}},
                        {vote.created,
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
            {'pers.citizen_room_id', proplists:get_value(id, Params, null)}
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
                                    false  ->
                                        doc.created;
                                    true ->
                                        doc.nvotes
                                end
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
            {distinct, [doc.id]},
            {fileinfotype_alias, download},
            {fields, [
                {as, {fileinfo.path, fileinfopath}},
                {as, {fileinfo.dir,  fileinfodir}}
                | proplists:delete(path, Fields)
            ]},
            {doc.isdeleted,false},
            {doc.isrepost,false},
            {doc.isrepostcont,false}
            |
            case Isweek of
                false  ->
                    [
                        {order, {desc, doc.created}}
                        | What_
                    ];
                true  ->
                    [
                        {order, {desc, doc.nvotes}},
                        {vote.created,
                            {gt, empdb_convert:now_minus_week()}
                        }
                        | What_
                    ]
            end
        ]) of
            {ok,Phobjs} ->
                {ok,
                    lists:map(fun({Phpl})->
                        case (lists:member(photo.path, Fields) or (Fields =:= [])) of
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




    
get(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_daowp_room:get(Con, Params)
    end).

get(Params, Fields)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_daowp_room:get(Con, Params, Fields)
    end).


is_owner(Uid, Oid)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_room:is_owner(Con, Uid, Oid)
    end).

%%
%% Local Functions
%%

