-module(dao).


-include_lib("epgsql/include/pgsql.hrl").

-include("empdb.hrl").

-compile(export_all).

-define(SVAR, "$").

%%%
%%% Спецификации
%%%

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {table,     0},
            %% ::= table(name)
        {table,     1},
            %% {fields, insert, required}
            %% {fields, insert}
            %% {fields, select}
            %% {fields, update}
            %% {fields, all}
            %% name
        {create,    2},
        {update,    2},
        {get,       2},
        {get,       3},
        {is_owner,  2}
    ];
behaviour_info(_Other) ->
    undefined.


%%
%%
%% @spec id2alias(
%%      Object::atom(), 
%%      Id::integer(), 
%%      Options::proplists:proplist()
%% ) -> Alias::atom()
%% 
%% @param Object    atom()                  имя таблицы
%% @param Id        integer()               id объекта
%% @param Options   proplists:proplist()    настройки преобразования
%% 


id2alias(doctype, _, _options) ->
    null;

id2alias(contype, _, _options) ->
    null;
    
id2alias(oktype, 3, _options) ->
    ok;

id2alias(oktype, _, _options) ->
    null;

id2alias(_object, _id, _options) ->
    undefined.

%%
%%
%% @spec id2alias(
%%      Object::atom(), 
%%      Alias::atom(), 
%%      Options::proplists:proplist()
%% ) -> Alias::atom()
%% 
%% @param Object    atom()                  имя таблицы
%% @param Alias     atom()                  id объекта
%% @param Options   proplists:proplist()    настройки преобразования
%% 


alias2id(doctype, _, _options) ->
    null;

alias2id(contype, _, _options) ->
    null;
    
alias2id(oktype, ok, _options) ->
    3;

alias2id(oktype, _, _options) ->
    null;

alias2id(_object, _alias, _options) ->
    null.


%%% -----------------------------------------------------------------------

fields(all) ->
    fields([], all);

fields(Fields) ->
    fields(Fields, []).

fields(Fields, all) ->
    fields(Fields, <<"*">>);

fields(Fields, Default) ->
    fields(Fields, Default, []).

fields(all, Default, Additions) ->
    fields_([], [], Default, Additions);

fields(Fields, Default, Additions) ->
    fields_([], Fields, Default, Additions).
    
%%% -----------------------------------------------------------------------

table_fields(Table, Fields) ->
    table_fields(Table, Fields, []).

table_fields(Table, Fields, Default) ->
    table_fields(Table, Fields, Default, []).

table_fields(Table, Fields, Default, Additions) ->
    fields_([convert:to_binary(Table), <<".">>], Fields, Default, Additions).

%%% -----------------------------------------------------------------------

fields_(_, [], [], _additions) ->
   <<>>;

fields_(Table, [], [D|Rest] = Default, Additions) when erlang:is_atom(D) ->
    fields_(Table, Default, [], Additions);

fields_(_, [], Default, _additions) ->
   Default;

fields_(Table, Fields, _default, Additions) ->
    [[_|First]|Res] = lists:map(fun(Field)->
        [<<",">> ,[convert:to_binary(Table), convert:to_binary(Field)]]
    end,lists:append(Fields, Additions)),
    [<<" ">>, [First|Res], <<" ">>].

%%% -----------------------------------------------------------------------


fieldvars(Fields) ->
    fieldvars(Fields, []).

fieldvars(Fields, Default) ->
    fieldvars(Fields, Default, []).

fieldvars([], Default, _additions) ->
    Default;
fieldvars(Fields, _default, Additions) ->
    [[_|First]|Res] = lists:map(fun(Field)->
        [<<",">> , [<<"$">>, convert:to_binary(Field)]]
    end,lists:append(Fields, Additions)),
    [<<" ">>, [First|Res], <<" ">>].

%%% -----------------------------------------------------------------------

fields_fieldvars(Fields)->
    fields_fieldvars(Fields, []).

fields_fieldvars(Fields, Default)->
    fields_fieldvars(Fields, Default, []).

fields_fieldvars([], Default, _additions) ->
    Default;
fields_fieldvars(Fields, _default, Additions) ->
    [[_|First]|Res] = lists:map(fun(Field)->
        [<<",">> , [convert:to_binary(Field), <<"=$">>, convert:to_binary(Field)]]
    end,Fields),
    [<<" ">>, [First|Res], <<" ">>].

    
    

fieldtuples_fieldvars(Fields)->
    fieldtuples_fieldvars(Fields, []).

fieldtuples_fieldvars(Fields, Default)->
    fieldtuples_fieldvars(Fields, Default, []).

fieldtuples_fieldvars([], Default, _additions) ->
    Default;
    
fieldtuples_fieldvars(Fields, _default, Additions) ->
    [[_|First]|Res] = lists:map(
        fun
            ({Field, {incr, Num}})->
                [   <<",">> , 
                    [   convert:to_binary(Field), 
                        <<" = ">>, 
                        convert:to_binary(Field),
                        <<" + ">>, 
                        convert:to_list(Num)
                    ]
                ];
            ({Field, {decr, Num}})->
                [   <<",">> , 
                    [   convert:to_binary(Field), 
                        <<" = ">>, 
                        convert:to_binary(Field),
                        <<" - ">>, 
                        convert:to_list(Num)
                    ]
                ];
            ({incr, {Field, Num}})->
                [   <<",">> , 
                    [   convert:to_binary(Field), 
                        <<" = ">>, 
                        convert:to_binary(Field),
                        <<" + ">>, 
                        convert:to_list(Num)
                    ]
                ];
            ({decr, {Field, Num}})->
                [   <<",">> , 
                    [   convert:to_binary(Field), 
                        <<" = ">>, 
                        convert:to_binary(Field),
                        <<" - ">>, 
                        convert:to_list(Num)
                    ]
                ];
            ({Field, _})->
                [   <<",">> , 
                    [   convert:to_binary(Field), 
                        <<"=$">>, 
                        convert:to_binary(Field)
                    ]
                ]
        end,
    Fields
    ),
    [<<" ">>, [First|Res], <<" ">>].

    
%%% -----------------------------------------------------------------------
%%% -----------------------------------------------------------------------
%%% -----------------------------------------------------------------------

sql_and(List)->
    {Tlist, String} = sql_list(List),
    {   Tlist,
        [   <<"(">>,
                string:join(String,[<<" and ">>]),
            <<")">>
        ]
    }.

sql_or(List)->
    {Tlist, String} = sql_list(List),
    {   Tlist,
        [   <<"(">>,
                string:join(String,[<<" or ">>]),
            <<")">>
        ]
    }.

sql_list(List) ->
    io:format("List = ~p~n", [List]),
    {Tlist, String} = lists:unzip([sql_cond({Ff, Val})||{Ff, Val} <- List]),
    io:format("String = ~p~n", [String]),
    {lists:flatten(Tlist), String}.

sql_cond({'and', List}) ->
    sql_and(List);

sql_cond({'or', List}) ->
    sql_or(List);

sql_cond({Ff, {[{X, C}]}})->
    io:format("{Ff, {X, C}} = ~p~n", [ {Ff, {X, C}}]),
    sql_cond({Ff, {X, C}});

sql_cond({Ff, {X, C}}) when erlang:is_list(X) orelse erlang:is_binary(X)->
    io:format("{Ff, {X, C}} = ~p~n", [ {Ff, {X, C}} ]),
    sql_cond({Ff, {convert:to_atom(X), C}});
    
sql_cond({Ff, {'and', Conds}})->
    X =
        {   'and',
            lists:map(
                fun(Cond)->
                    {Ff, Cond}
                end,
                Conds
            )
        },
    sql_cond(X);

sql_cond({Ff, {'or', Conds}})->
    io:format("Conds = ~p~n", [Conds]),
    X =
        {   'or',
            lists:map(
                fun(Cond)->
                    {Ff, Cond}
                end,
                Conds
            )
        },
    sql_cond(X);

sql_cond({Ff, {between, [Left, Right]}}) ->
    Bnleft   = convert:to_binary([
        <<"__">>,
        convert:to_binary(Ff),
        <<"__left__">>
    ]),
    Bnright  = convert:to_binary([
        <<"__">>,
        convert:to_binary(Ff),
        <<"__right__">>
    ]),
    Nleft    = convert:to_atom(Bnleft),
    Nright   = convert:to_atom(Bnright),
    {   [{Nleft,Left},{Nright, Right}],
        [   convert:to_binary(Ff),
            <<" between ">>, ["$",Bnleft], <<" and ">>, ["$",Bnright]
        ]
    };

sql_cond({Ff, {iregex, Regex}})
    when erlang:is_binary(Regex) orelse erlang:is_list(Regex) ->
    {   [],
        [   convert:to_binary(Ff),
            <<" ~* '">>,
            convert:to_binary(Regex),
            <<"'">>
        ]
    };

sql_cond({Ff, {regex, Regex}})
    when erlang:is_binary(Regex) orelse erlang:is_list(Regex) ->
    {   [],
        [   convert:to_binary(Ff),
            <<" ~ '">>,
            convert:to_binary(Regex),
            <<"'">>
        ]
    };

sql_cond({Ff, {startswith, Startswith}}) ->
    sql_cond({Ff, {like, [<<"%">>, Startswith]}});

sql_cond({Ff, {endswith, Endswith}}) ->
    sql_cond({Ff, {like, [Endswith, <<"%">>]}});

sql_cond({Ff, {contains, Contains}}) ->
    sql_cond({Ff, {like, [<<"%">>, Contains, <<"%">>]}});

sql_cond({Ff, {like, Like}})
    when erlang:is_binary(Like) orelse erlang:is_list(Like) ->
    {   [],
        [   convert:to_binary(Ff),
            <<" like '">>,
            convert:to_binary(Like),
            <<"'">>
        ]
    };

sql_cond({Ff, {istartswith, Startswith}}) ->
    sql_cond({Ff, {ilike, [<<"%">>, Startswith]}});

sql_cond({Ff, {iendswith, Endswith}}) ->
    sql_cond({Ff, {ilike, [Endswith, <<"%">>]}});

sql_cond({Ff, {icontains, Contains}}) ->
    sql_cond({Ff, {ilike, [<<"%">>, Contains, <<"%">>]}});

sql_cond({Ff, {ilike, Like}})
    when erlang:is_binary(Like) orelse erlang:is_list(Like) ->
    {   [],
        [   convert:to_binary(Ff),
            <<" ilike '">>,
            convert:to_binary(Like),
            <<"'">>
        ]
    };

sql_cond({Ff, {lt, Val}}) ->
    {   [{cond_atom(Ff), Val}],
        [   convert:to_binary(Ff),
            <<" < $">>,
            convert:to_binary(cond_atom(Ff))
        ]
    };

sql_cond({Ff, {lte, Val}}) ->
    {[{cond_atom(Ff), Val}], [convert:to_binary(Ff),<<" <= $">>,convert:to_binary(cond_atom(Ff))]};

sql_cond({Ff, {gt, Val}}) ->
    {[{cond_atom(Ff), Val}], [convert:to_binary(Ff),<<" > $">>,convert:to_binary(cond_atom(Ff))]};

sql_cond({Ff, {gte, Val}}) ->
    {[{cond_atom(Ff), Val}], [convert:to_binary(Ff),<<" >= $">>,convert:to_binary(cond_atom(Ff))]};

sql_cond({Ff, {in, []}}) ->
    {[], []};

sql_cond({Ff, {in, List}}) when erlang:is_list(List) ->
    {[], [
        convert:to_binary(Ff),
        <<" in (">>,
        string:join(
            [[convert:to_binary(Li)]|| Li <- List],
            [<<",">>]
        ),
        <<")">>
    ]};


    
sql_cond({Ff, Val} = Tuple) ->
    io:format("Tuple = ~p~n~n", [Tuple]),
    {[{cond_atom(Ff), Val}], [
        convert:to_binary(Ff),
        <<" = $">>,
        convert:to_binary(cond_atom(Ff))
    ]}.

cond_atom(Ff)->
    erlang:list_to_atom("`" ++ erlang:atom_to_list(Ff) ++ "@filter`").

    
sql_where(<<>>)->
    {[], []};

sql_where(undefined)->
    {[], []};

sql_where([])->
    {[], []};

sql_where(Current_all_fields)->
    {PField, Pstring} = sql_and(Current_all_fields),
    {PField, [<<" where ">>, Pstring]}.



sql_limit([])->
    [];

sql_limit(<<>>)->
    [];

sql_limit(undefined)->
    [];

sql_limit(Limit)->
    [   <<" limit ">>,
        convert:to_list(Limit),
        <<" ">>
    ].


sql_offset([])->
    [];

sql_offset(<<>>)->
    [];

sql_offset(undefined)->
    [];

sql_offset(Limit)->
    [   <<" offset ">>,
        convert:to_list(Limit),
        <<" ">>
    ].


sql_order([])->
    [];

sql_order(undefined)->
    [];

sql_order(O)
    when erlang:is_atom(O)
    orelse erlang:is_binary(O) ->
    sql_order([O]);

sql_order({O, D}) ->
    sql_order([{O, D}]);

sql_order(Order) when erlang:is_list(Order) ->
    [   <<" order by ">>,
        string:join(
            [[sql_order_one(O)] ||O <- Order],
            [<<",">>]
        ),
        <<" ">>
    ].

sql_order_one({[{D, O}]}) ->
    sql_order_one({D, O});

sql_order_one({D, O})
    when (
        erlang:is_atom(O)
        orelse erlang:is_binary(O)
    ) andalso (
        D =:= asc orelse D=:= desc
    )->
    [   convert:to_binary(O),
        <<" ">>,
        convert:to_binary(D)
    ];

sql_order_one({O, D})
    when (
        erlang:is_atom(O)
        orelse erlang:is_binary(O)
    ) andalso (
        D =:= asc orelse D=:= desc
    )->
    [   convert:to_binary(O),
        <<" ">>,
        convert:to_binary(D)
    ];

sql_order_one(O) ->
    sql_order_one({asc, O}).

% 
% sql_order([O|Rorder] = Order, Direction)
%     when (
%         erlang:is_atom(O)
%         orelse erlang:is_list(O)
%         orelse erlang:is_binary(O)
%     ) andalso (
%         Direction =:= asc orelse Direction =:= desc 
%     )->
%     [   <<" order by ">>,
%         string:join(
%             [[convert:to_binary(O)]||O<- Order],
%             [<<",">>]
%         ),
%         <<" ">>, convert:to_binary(Direction), <<" ">>
%     ];

% sql_order(Order, Direction)
%     when (
%         erlang:is_atom(Order)
%         orelse erlang:is_list(Order)
%         orelse erlang:is_binary(Order)
%     ) andalso (
%         Direction =:= asc orelse Direction =:= desc
%     )->
%     [   <<" order by  ">>,
%         convert:to_binary(Order),
%         <<" ">>,
%         convert:to_binary(Direction),
%         <<" ">>
%     ].


% case Order of
%                         [] ->
%                             [];
%                         undefined ->
%                             [];
%                         Order_list when erlang:is_list(Order_list )->
%                             [   <<" offset ">>,
%                                 string:join(
%                                     [convert:to_binary(O)||O<- Order_list],
%                                     [<<",">>]
%                                 ),
%                                 <<" ">>
%                             ]
%                     end,

sql_returning([]) ->
    [];

sql_returning(undefined) ->
    [];

sql_returning(<<>>) ->
    [];

sql_returning([{Fitem, _}|Rfilter] = Filter) ->
    [   <<" returning ">>,
        convert:to_binary(Fitem),
        lists:map(fun({Item, _}) ->
            [<<" ,">>, convert:to_binary(Item)]
        end,Rfilter)
    ];

sql_returning([Fitem|Rfilter] = Filter) ->
    [   <<" returning ">>,
        convert:to_binary(Fitem),
        lists:map(fun(Item) ->
            [<<" ,">>, convert:to_binary(Item)]
        end,Rfilter)
    ];

sql_returning(Filter) ->
    [   <<" returning ">>,
        convert:to_binary(Filter)
    ].

%%% -----------------------------------------------------------------------
%%% -----------------------------------------------------------------------
%%% -----------------------------------------------------------------------



table_options({table, fields, all},      Current) ->
    [   'and', 'or'
        | proplists:get_value({table, fields, all}, Current, [])
    ];

table_options(Oname,      Current) ->
    proplists:get_value(Oname, Current).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Выполняет select. Позволяет получить одну сущность 
%%      единственной таблицы.
%%
%%      Операторы:
%%              and 
%%              or
%%
%%          Операторы надо строками, префикс i в начале означает,
%%          что регистр учитываться не будет:
%%
%%                  like,           ilike
%%                      Сопосталение в терминах sql.
%%                  regex,           iregex
%%                      Регулярные выражения.
%%                  contains,       icontains
%%                      Проверка на вхождение.
%%                  startswith,     istartswith
%%                      Проверка префикса.
%%                  endswith,       iendswith
%%                      Проверка суффикса.
%%                  between
%%                      Проверка вхождения.
%%
%%      Примеры:
%%          ----------------------------------------------------------------
%%          dao_some:get(Connection,
%%              [ 
%%                  {'or', 
%%                      [
%%                          {name, {contains, <<"lennon">>}},
%%                          {name, {startswith, <<"jo">>}}
%%                      ]
%%                  }
%%              ]
%%          ).
%%          ----------------------------------------------------------------
%%          dao_some:get(Connection,
%%              [
%%                  {'fname',
%%                      {'or', 
%%                          [
%%                              {contains, <<"lennon">>}, 
%%                              {startswith, <<"jo">>}
%%                          ]
%%                      }
%%                  }
%%              ]
%%          ).
%%          ----------------------------------------------------------------
%%          dao_some:get(Connection, [{id, {'between', {3, 33}}}]).
%%          ----------------------------------------------------------------
%%
%%      Зарезервированные слова: limit, offset
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(Current, Con, #queryobj{order=Order}=Obj)
    when erlang:is_atom(Order) orelse erlang:is_tuple(Order) ->
    io:format("Order = ~p~n~n", [Order]),
    get(Current, Con, Obj#queryobj{order=[Order]});

get(Current, Con, #queryobj{filter=Filter}=Obj)
    when erlang:is_atom(Filter) orelse erlang:is_tuple(Filter) ->
    io:format("Filter = ~p~n~n", [Filter]),
    get(Current, Con, Obj#queryobj{filter=[Filter]});


get(Current, Con, #queryobj{fields=Fields}=Obj)
    when erlang:is_atom(Fields) orelse erlang:is_tuple(Fields) ->
    io:format("Fields = ~p~n~n", [Fields]),
    get(Current, Con, Obj#queryobj{fields=[Fields]});
    
%%
%% TODO: только для двух таблиц
%%
get([{Parent, Parent_field}, {Current, Current_field}], Con, #queryobj{
    filter  =   Filter,
    fields  =   Fields,
    order   =   Order,
    limit   =   Limit,
    offset  =   Offset
}) when erlang:is_list(Filter), erlang:is_list(Current),erlang:is_list(Parent)->
    Common_all_fields = lists:append(
        table_options({table, fields, all},      Parent),
        table_options({table, fields, all},      Current)
    ),
    Common_select_fields = lists:append(
        table_options({table, fields, select},   Parent),
        table_options({table, fields, select},   Current)
    ),
    Current_select_fields =
        lists:filter(
            fun(F)->
                lists:member(F, Common_select_fields)
            end,
            Fields
        ),
    Current_all_fields =
        lists:filter(
            fun({F, _})->
                lists:member(F, Common_all_fields)
            end,
            Filter
        ),
    Current_order =
        lists:filter(
            fun(F)->
                lists:member(F, Common_all_fields)
            end,
            Order
        ),

    io:format("Fields = ~p~n~n", [Fields]),
    io:format("Current_select_fields = ~p~n~n", [Current_select_fields]),
    
    Binary_parent_name =
        convert:to_binary(table_options({table, name},Parent)),
    Binary_table_name =
        convert:to_binary(table_options({table, name},Current)),
    Binary_select_fields =
        fields(
            Current_select_fields,
            Common_select_fields
        ),
    Binary_current_field =
        convert:to_binary(Current_field),
    Binary_parent_field =
        convert:to_binary(Parent_field),
    {Pfields, Where_string} =
        sql_where(Current_all_fields),
    Query = [
        %% поля обоих таблиц в перемешку
        <<" select ">>, Binary_select_fields,
        %% родительская таблиа
        <<" from ">>,   Binary_parent_name,
        %% дочерняя таблиа
        <<" join ">>,   Binary_table_name,
        %% сцепление таблиц
        <<" on ">>, [
            Binary_table_name,  <<".">>,    Binary_current_field,
            <<" =  ">>,
            Binary_parent_name, <<".">>,    Binary_parent_field
        ],
        Where_string,
        [
            sql_order(Current_order),
            sql_limit(Limit),
            sql_offset(Offset)
        ]
    ],
    dao:pgret(dao:equery(Con, Query, Pfields));


get([{Parent, Parent_field}, {Current, Current_field}],Con,#queryobj{}=Qo)->
    Oparent = [
        {{table, name},             Parent:table(name)},
        {{table, fields, all},      Parent:table({fields, all})},
        {{table, fields, select},   Parent:table({fields, select})}
    ],
    Ocurrent = [
        {{table, name},             Current:table(name)},
        {{table, fields, all},      Current:table({fields, all})},
        {{table, fields, select},   Current:table({fields, select})}
    ],
    io:format("Oparent = ~p~n", [Oparent]),
    io:format("Ocurrent = ~p~n", [Ocurrent]),
    get([{Oparent, Parent_field}, {Ocurrent, Current_field}],Con,Qo);

get(Current, Con, #queryobj{
    filter  =   Filter,
    fields  =   Fields,
    order   =   Order,
    limit   =   Limit,
    offset  =   Offset
}) when erlang:is_list(Filter), erlang:is_list(Current) ->
    Common_all_fields =
        table_options({table, fields, all}, Current),
    Common_select_fields =
        table_options({table, fields, select}, Current),
    Current_select_fields =
        lists:filter(
            fun(F)-> lists:member(F, Common_select_fields) end,
            Fields
        ),
    Current_all_fields =
        lists:filter(
            fun({F, _})->
                lists:member(F, Common_all_fields)
            end,
            Filter
        ),
    Current_order =
        lists:filter(
            fun
                (F)->
                    lists:member(F, Common_all_fields)
            end,
            Order
        ),
    io:format("Order = ~p~n~n", [Current_order]),
    
    Binary_table_name =
        convert:to_binary(table_options({table, name},  Current)),
    Binary_select_fields =
        fields(
            Current_select_fields,
            Common_select_fields
        ),
    {Pfields, Where_string} =
        sql_where(Current_all_fields),
    Query = [
        <<"select ">>,  Binary_select_fields,
        <<" from ">>,   Binary_table_name,
        Where_string,
        [
            sql_order(Current_order),
            sql_limit(Limit),
            sql_offset(Offset)
        ]
    ],
    dao:pgret(dao:equery(Con, Query, Pfields));


get(Current,Con,#queryobj{}=Qo)  ->
    Ocurrent = [
        {{table, name},             Current:table(name)},
        {{table, fields, all},      Current:table({fields, all})},
        {{table, fields, select},   Current:table({fields, select})}
    ],
    io:format("Ocurrent = ~p~n", [Ocurrent]),
    get(Ocurrent,Con,Qo);

get(Current, Con, Opts) when erlang:is_list(Opts) ->
    As_filter =
        case proplists:get_value(filter, Opts, undefined) of
            undefined   ->    Opts;
            Filter      ->    Filter
        end,
    io:format("proplists:get_value(fields,  Opts) = ~p ~n~n", [proplists:get_value(fields,  Opts)]),
    get(Current, Con,
        #queryobj{
                filter  =   As_filter,
                fields  =
                    proplists:get_value(fields,  Opts,
                        proplists:get_value(returning,  Opts,
                            proplists:get_value(return,  Opts, [])
                        )
                    ),
                order   =   proplists:get_value(order,   Opts, []),
                limit   =   proplists:get_value(limit,   Opts),
                offset  =   proplists:get_value(offset,  Opts)
        }
    ).

get(Current,Con,Opts,Fields) when erlang:is_list(Opts) ->
    io:format("Current = ~p~n", [Current]),
    
    As_filter =
        case proplists:get_value(filter, Opts, undefined) of
            undefined   ->    Opts;
            Filter      ->    Filter
        end,
    get(Current, Con,
        #queryobj{
                filter  =   As_filter,
                fields  =   Fields,
                order   =   proplists:get_value(order,   Opts, []),
                limit   =   proplists:get_value(limit,   Opts),
                offset  =   proplists:get_value(offset,  Opts)
        }
    ).

% get(    Current,
%         Con,
%         {Key, Value},
%         Fields,
%         Limit,
%         Offset
% )->
%     get(Current, Con, [{Key, Value}], Fields);



% 
% %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% @doc Выполняет select join для таблиц описанных через
% %%      {Parent, Parent_field}, {Current, Current_field}
% %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% % get(Parent, Current, Con, Opts#queryobj{
% %         filter  =   Filter,
% %         fields  =   Fields,
% %         limit   =   Limit,
% %         offset  =   Offset
% %     })->
% %     get(Current,Con,Filter,Fields,Limit,Offset);
% % 
% % get([Parent, Current], Con, Opts) when erlang:is_list(Opts) ->
% %     As_filter =
% %         case proplists:get_value(filter, Opts, undefined) of
% %             undefined   ->    Opts;
% %             Filter      ->    Filter
% %         end,
% %     get(Parent, Current, Con, As_filter,
% %         proplists:get_value(fields,  Opts, []),
% %         proplists:get_value(limit,   Opts, []),
% %         proplists:get_value(offset,  Opts, []),
% %     ).
% % 
% % get([Parent, Current], Con, Opts, Fields) when erlang:is_list(Opts) ->
% %     As_filter =
% %         case proplists:get_value(filter, Opts, undefined) of
% %             undefined   ->    Opts;
% %             Filter      ->    Filter
% %         end,
% %     get([Parent, Current], Con, As_filter, Fields,
% %         proplists:get_value(limit,   Opts, []),
% %         proplists:get_value(offset,  Opts, []),
% %     ).
% % 
% % get(Par, Cur, Con, {Key, Value}, Fields,Limit,Offset)->
% %     get(Par, Cur, Con, [{Key, Value}], Fields,Limit,Offset).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Добавление нового
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


create(Current, Con, #queryobj{values=Values}=Obj)
    when erlang:is_atom(Values) orelse erlang:is_tuple(Values) ->
    io:format("Values = ~p~n~n", [Values]),
    create(Current, Con, Obj#queryobj{order=[Values]});

create(Current, Con, #queryobj{fields=Fields}=Obj)
    when erlang:is_atom(Fields) orelse erlang:is_tuple(Fields) ->
    io:format("Fields = ~p~n~n", [Fields]),
    create(Current, Con, Obj#queryobj{fields=[Fields]});
    
    
%%
%% TODO: только для двух таблиц
%%
create([{Parent, Parent_field}, {Current, Current_field}], Con, #queryobj{
    values  =   Values,
    fields  =   Returning
} = Queryobj)->
    case create(Parent,Con,Queryobj#queryobj{
        fields=[Parent_field|Returning]
    }) of
        {ok, [{Parent_pl}]} ->
            Pid = proplists:get_value(Parent_field, Parent_pl),
            case create(Current, Con, Queryobj#queryobj{
                values=[{Current_field, Pid}|Values]
            }) of
                {ok, [{Child_pl}]} ->
                    {   ok,
                        [
                            {   lists:append(
                                    Parent_pl,
                                    proplists:delete(
                                        Current_field,
                                        Child_pl
                                    )
                                )
                            }
                        ]
                    };
                {Eclass, Error} ->
                    {Eclass, Error}
            end;
        {Eclass, Error} ->
            {Eclass, Error}
    end;

create(Current, Con, #queryobj{
    values  =   Values,
    fields  =   Returning
}) when erlang:is_list(Current)->
    Common_select_fields =
        table_options({table, fields, select}, Current),
    Common_insert_fields =
        table_options({table, fields, insert}, Current),
    Common_required_fields =
        table_options({table, fields, insert, required}, Current),
        
    io:format("Common_select_fields = ~p~n~n", [Common_select_fields]),
    io:format("Returning = ~p~n~n", [Returning]),
    
    Current_select_fields =
        lists:filter(
            fun(F)-> lists:member(F, Common_select_fields) end,
            Returning
        ),
    Current_insert_fields = lists:filter(
        fun({F, _})->
            lists:member(F, Common_insert_fields)
        end,
        Values
    ),
    Current_returning_fields = 
        case Current_select_fields of
            [] ->
                case lists:member(id, Common_select_fields) of
                    true ->
                        [id];
                    _ ->
                        [X|_] = Common_select_fields, [X]
                end;
            _ ->
                Current_select_fields
        end,
    Current_insert_fields_keys =
        proplists:get_keys(Current_insert_fields),
    Has_required =
        lists:foldl(
            fun(F, R)->
                R and lists:keymember(F, 1, Current_insert_fields)
            end,
            true,
            Common_required_fields
        ),

    io:format("Current_insert_fields = ~p~n~n", [Current_insert_fields]),
    io:format("Common_required_fields = ~p~n~n", [Common_required_fields]),
    
    case {Has_required, Current_insert_fields_keys} of
        {true, []} ->
            {error, {empty, Common_required_fields}};
        {true, _} ->
            Binary_table_name    = convert:to_binary(
                table_options({table, name}, Current)
            ),
            Binary_insert_fields_keys =
                fields(Current_insert_fields_keys),
            Binary_insert_fields_vars =
                fieldvars(Current_insert_fields_keys),
            Query = [
                <<"insert into ">>, Binary_table_name,
                <<"(">>,            Binary_insert_fields_keys, <<") ">>,
                <<"values(">>,      Binary_insert_fields_vars, <<") ">>,
                sql_returning(Current_returning_fields)
            ],
            dao:pgret(dao:equery(Con, Query, Current_insert_fields));
        _ ->
            {error, {required, Common_required_fields}}
    end;

create(Current, Con, #queryobj{}=Queryobj)->
    Ocurrent = [
        {   {table, name},
            Current:table(name)
        },
        {   {table, fields, select},
            Current:table({fields, select})
        },
        {   {table, fields, insert},
            Current:table({fields, insert})
        },
        {   {table, fields, insert, required},
            Current:table({fields, insert, required})
        }
    ],
    create(Ocurrent, Con, Queryobj);

create(Current, Con, Opts) when erlang:is_list(Opts) ->
    io:format("Opts = ~p~n~n", [Opts]),
    As_values =
        case proplists:get_value(values, Opts, undefined) of
            undefined   ->    Opts;
            Values      ->    Values
        end,
    create(Current, Con, #queryobj{
        values  =   As_values,
        fields  =
            proplists:get_value(fields,  Opts,
                proplists:get_value(returning,  Opts,
                    proplists:get_value(return,  Opts, [])
                )
            )
    }).

create(Current, Con, Opts, Fields) when erlang:is_list(Opts) ->
    As_values =
        case proplists:get_value(values, Opts, undefined) of
            undefined   ->    Opts;
            Values      ->    Values
        end,
    create(Current, Con, #queryobj{
        values  =   As_values,
        fields  =   Fields
    }).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Обновление старого
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


update(Current, Con, #queryobj{values=Values}=Obj)
    when erlang:is_atom(Values) orelse erlang:is_tuple(Values) ->
    io:format("Values = ~p~n~n", [Values]),
    update(Current, Con, Obj#queryobj{order=[Values]});

update(Current, Con, #queryobj{filter=Filter}=Obj)
    when erlang:is_atom(Filter) orelse erlang:is_tuple(Filter) ->
    io:format("Filter = ~p~n~n", [Filter]),
    update(Current, Con, Obj#queryobj{filter=[Filter]});

update(Current, Con, #queryobj{fields=Fields}=Obj)
    when erlang:is_atom(Fields) orelse erlang:is_tuple(Fields) ->
    io:format("Fields = ~p~n~n", [Fields]),
    update(Current, Con, Obj#queryobj{fields=[Fields]});
    
    
update([{Parent, Parent_field}, {Current, Current_field}], Con, #queryobj{
    values  =   Values,
    filter  =   Filter,
    fields  =   Returning
} = Queryobj)->
    case update(Parent,Con,Queryobj#queryobj{
        fields=[Parent_field|Returning]
    }) of
        {ok, [{[]}]} ->
            Pid_pl =
                case proplists:get_value(Parent_field, Filter) of
                    undefined ->
                        [];
                    Pid ->
                        [{Current_field, Pid}]
                end,
            case update(Current, Con, Queryobj#queryobj{
                filter=lists:append(Pid_pl, Filter)
            }) of
                {ok, [{Child_pl}]} ->
                    {ok, [{lists:append([], Child_pl)}]};
                {Eclass, Error} ->
                    {Eclass, Error}
            end;
        {ok, [{Parent_pl}]} ->
            Pid = proplists:get_value(Parent_field, Parent_pl),
            case update(Current, Con, Queryobj#queryobj{
                filter=[{Current_field, Pid}|Filter]
            }) of
                {ok, [{Child_pl}]} ->
                    {ok, [{lists:append(Parent_pl, Child_pl)}]};
                {Eclass, Error} ->
                    {Eclass, Error}
            end;
        {Eclass, Error} ->
            {Eclass, Error}
    end;

update(Current, Con, #queryobj{
    values  =   Values,
    filter  =   Filter,
    fields  =   Returning
} = Queryobj) when erlang:is_list(Current) ->
    Common_all_fields =
        table_options({table, fields, all}, Current),
    Common_select_fields =
        table_options({table, fields, select}, Current),
    Common_update_fields =
        table_options({table, fields, update}, Current),
    Current_select_fields =
        lists:filter(
            fun(F)-> lists:member(F, Common_select_fields) end,
            Returning
        ),
%     io:format("Returning = ~p~n~n~n", [Returning]),
%     io:format("Current_select_fields = ~p~n~n~n", [Current_select_fields]),
    Current_all_fields =
        lists:filter(
            fun({F, _})->
                lists:member(F, Common_all_fields)
            end,
            Filter
        ),
    Current_update_fields =
        lists:filter(
            fun
                ({incr, {F, _}})->
%                     io:format("F = ~p~n~n~n", [F]),
                    lists:member(F, Common_update_fields);
                ({decr, {F, _}})->
%                     io:format("F = ~p~n~n~n", [F]),
                    lists:member(F, Common_update_fields);
                ({F, _})->
%                     io:format("F = ~p~n~n~n", [F]),
                    lists:member(F, Common_update_fields)
            end,
            Values
        ),
% 
%     io:format("Values = ~p~n~n~n", [Values]),
%     io:format("Common_update_fields = ~p~n~n~n", [Common_update_fields]),
%     
%     io:format("Current_update_fields = ~p~n~n~n", [Current_update_fields]),
    case Current_update_fields of
        [] ->
            %{ok, [{[]}]};
            case Current_select_fields of
                [] -> 
                    {ok, [{[]}]};
                _ ->    
                    get(Current, Con, #queryobj{
                        filter=Filter, 
                        fields=Current_select_fields
                    })
            end;
        _ ->
            Current_returning_fields =
                case Current_select_fields of
                    [] ->
                        case lists:member(id, Common_select_fields) of
                            true ->
                                [id];
                            _ ->
                                [X|_] = Common_select_fields, [X]
                        end;
                    _ ->
                        Current_select_fields
                end,
            Binary_table_name = convert:to_binary(
                table_options({table, name}, Current)
            ),
            Binary_update_fields = fieldtuples_fieldvars(Current_update_fields),
            {Pfields, Where_string} =
                sql_where(Current_all_fields),
            Query = [
                <<" update  ">>,
                    Binary_table_name,
                <<" set ">>,
                    Binary_update_fields,
                Where_string,
                sql_returning(Current_returning_fields)
            ],
            io:format("Q = ~p~n", [Query]),
            case dao:pgret(
                dao:equery(
                    Con,Query,lists:append(Current_update_fields, Pfields)
                )
            ) of
                {ok, 0} ->
                    {ok, []};
                    %create(Current, Con, Queryobj);
                Result ->
                    Result
            end
    end;

update(Current, Con, #queryobj{}=Queryobj)->
    Ocurrent = [
        {   {table, name},
            Current:table(name)
        },
        {   {table, fields, all},
            Current:table({fields, all})
        },
        {   {table, fields, select},
            Current:table({fields, select})
        },
        {   {table, fields, insert},
            Current:table({fields, insert})
        },
        {   {table, fields, insert, required},
            Current:table({fields, insert, required})
        },
        {   {table, fields, update},
            Current:table({fields, update})
        }
    ],
    update(Ocurrent, Con, Queryobj);

update(Current, Con, Opts) when erlang:is_list(Opts) ->
    As_values =
        case proplists:get_value(values, Opts, undefined) of
            undefined ->
                Opts;
            Values ->
                Values
        end,
    As_filter =
        case {
            proplists:get_value(filter, Opts, undefined),
            proplists:get_value(id,     Opts, undefined)
        } of
            {undefined, undefined}   ->
                [];
            {undefined, Id}   ->
                [{id, Id}];
            {Filter, _}   ->
                Filter
        end,
    update(Current, Con, #queryobj{
        values  =   As_values,
        filter  =   As_filter,
        fields  =
            proplists:get_value(fields,  Opts,
                proplists:get_value(returning,  Opts,
                    proplists:get_value(return,  Opts, [])
                )
            )
    }).

update(Current, Con, Opts, Fields) when erlang:is_list(Opts) ->
    As_values =
        case proplists:get_value(values, Opts, undefined) of
            undefined ->
                Opts;
            Values ->
                Values
        end,
    As_filter =
        case {
            proplists:get_value(filter, Opts, undefined),
            proplists:get_value(id,     Opts, undefined)
        } of
            {undefined, undefined}   ->
                [];
            {undefined, Id}   ->
                [{id, Id}];
            {Filter, _}   ->
                Filter
        end,
    update(Current, Con, #queryobj{
        values  =   As_values,
        filter  =   As_filter,
        fields  =   Fields
    }).

%%% -----------------------------------------------------------------------
%%% -----------------------------------------------------------------------




pg2rs({ok, _, Vals}, Record_name) ->
    [list_to_tuple([Record_name | tuple_to_list(X)]) || X <- Vals];

pg2rs(Vals, Record_name) ->
    [list_to_tuple([Record_name | tuple_to_list(X)]) || X <- Vals].

strip_rs(Vals) when is_list(Vals)->
    strip_rs(Vals, []);

strip_rs(Val) when is_tuple(Val) ->
    [_|T]=tuple_to_list(Val),
    T.

strip_rs([Vh|Vt], Ret) ->
    [_|T]=tuple_to_list(Vh),
    strip_rs(Vt, [list_to_tuple(T)|Ret]);
strip_rs([], Ret) ->
    Ret.

collect_where_params(Params) ->
    collect_where_params([], [], Params).

collect_where_params(Where, Params, [{Val}|T]) when is_list(Val) ->
    collect_where_params([Val|Where], Params, T);
collect_where_params(Where, Params, [{Key, Val}|T]) ->
    collect_where_params(Where, Params, [{Key, "=", Val}|T]);
%    collect_where_params([lists:append([Key, " = $", convert:to_list(length(Params) + 1)])|Where], [Val | Params], T);
collect_where_params(Where, Params, [{Key, Action, Val}|T]) ->
    collect_where_params([lists:append([Key, " ", Action, " $", convert:to_list(length(Params) + 1)])|Where], [Val | Params], T);
collect_where_params(Where, Params, []) when length(Where) > 0->
    {" WHERe", [string:join(lists:reverse(Where), " and ")], lists:reverse(Params)};
collect_where_params(_, _, []) ->
    {";", []}.


to_type(null, _Type) ->
    null;


to_type(V,  timestamp) ->
    Bas = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    {X, {H, M, S}} = V,
    Ts = trunc(S),
    Cur = calendar:datetime_to_gregorian_seconds({X, {H, M, Ts}}),
    trunc((Cur - Bas + S - Ts) * 1000000);

to_type(V, int4) ->
    convert:to_integer(V);

to_type(V, numeric) ->
    convert:to_integer(V);

to_type(<<"f">>, bool) ->
    false;
to_type(<<"t">>, bool) ->
    true;

to_type(V, varchar) ->
    V;
to_type(V, text) ->
    V;
to_type(V, Type) ->
    io:format("Type= ~p~n", [Type]),
    V.


% name_columns([{column, Name, Type, _P3, _P4, _P5}|Ct], [V|Vt], Ret) ->
%     name_columns(Ct, Vt, [{binary_to_list(Name), to_type(V, Type)}|Ret]);
    
name_columns([{column, Name, Type, _P3, _P4, _P5}|Ct], [V|Vt], Ret) ->
    name_columns(
        Ct,
        Vt,
        [
            {
                erlang:list_to_atom(
                    erlang:binary_to_list(Name)
                ),
                to_type(V, Type)
            }
            | Ret
        ]
    );
    
name_columns([], [], Ret) ->
    {Ret};
name_columns([], V, Ret) ->
    %?D("unexpected values: ~p~n", [V]),
    {Ret};
name_columns(C, [], Ret) ->
    %?D("unexpected columns: ~p~n", [C]),
    {Ret}.

make_proplist(Columns, [V|T], Ret) ->
    make_proplist(Columns, T, [name_columns(Columns, tuple_to_list(V), [])|Ret]);
make_proplist(_C, [], Ret) ->
    Ret.

pgret(returning, {ok, Id}) ->
    
    {ok, Id};

pgret(returning, {ok, 1, _, [{Value}]}) ->

    {ok, Value};

pgret(_, Value) ->

    pgret(Value).

pgret({return, Value}) ->

    {ok, Value};

%%%
%%% select sq & eq
%%%
pgret({ok, Columns, Vals}) ->
    
    {ok, make_proplist(Columns, Vals, [])};


pgret([{ok, _columns, _vals}|_rest] = List) ->

    pgret_mult(List, []);
    
%%%
%%% update sq & eq
%%%
pgret({ok, Count}) ->
    {ok, Count};

%%%
%%% insert sq & eq
%%%
pgret({ok, 1, Columns, Vals}) ->
%     [{[{_,Res}]}] = make_proplist(Columns, Vals, []),
%     {ok, Res};
    {ok, make_proplist(Columns, Vals, [])};

%%%
%%% insert sq & eq
%%%
pgret({ok, _Count, Columns, Vals}) ->
    {ok, make_proplist(Columns, Vals, [])};

%%% 
%%% @doc    Ошибка сиквела - неожиданный возврат в функции
%%%         дает ошибку ожидаемого возврата.
%%%
pgret({pgcp_error, E}) ->

    pgreterr(E);

pgret({error, E}) ->

    pgreterr(E);

pgret(V) ->

    {retVal, V}.

pgret_mult([], Acc) ->
    Acc;
pgret_mult([{ok, Columns, Vals}|Rest], Acc) ->
    Res = make_proplist(Columns, Vals, []),
    pgret_mult(Rest, [{ok, Res}|Acc]).


pgreterr({error, E}) ->
    pgreterr(E);
pgreterr({badmatch, E}) ->
    pgreterr(E);
pgreterr(#error{code=Error_code_bin, message=Msg}) ->
    case Error_code_bin of
        <<"23502">> ->
            try
                {ok, Re} = re:compile("\"(.+)\""),
                {match, [_, C | _]} = re:run(Msg, Re, [{capture, all, list}]),
                {error, {not_null, erlang:list_to_binary(C)}}
            catch
                E:R ->
                    io:format("pgret ERROR(~p): ~p ~p - ~p~n", [?LINE, Msg, E, R]),
                    {error, {unknown, Msg}}
            end;
        <<"23503">> ->
            try
                {ok, Re} = re:compile("\"(.*?)*?_([^_].+)_fkey\""),
                {match, [_, _, C | _]} = re:run(Msg, Re, [{capture, all, list}]),
                {error, {not_exists, erlang:list_to_binary(C)}}
            catch
                E:R ->
                    io:format("pgret ERROR(~p): ~p ~p - ~p~n", [?LINE, Msg, E, R]),
                    {error, {unknown, Msg}}
            end;
        <<"23505">> ->
            try
                {ok, Reu} = re:compile("\"(.*?)*?_([^_].+)_key\""),
                {ok, Rep} = re:compile("\"(.*?)_pkey\""),
                case
                    {   re:run(Msg, Reu, [{capture, all, list}]),
                        re:run(Msg, Rep, [{capture, all, list}])
                    }
                of
                    {{match, [_, _, C | _]},_} ->
                        {error, {not_unique, erlang:list_to_binary(C)}};
                    {nomatch,{match,[_,C]}} ->
                        {error, {not_unique, erlang:list_to_binary(C)}}
                end
            catch
                E:R ->
                    io:format("pgret ERROR(~p): ~p ~p - ~p~n", [?LINE, Msg, E, R]),
                    {error, {unknown, Msg}}
            end;
        Code ->
            io:format("Code ~p ~n", [Code]),
            {error, {unknown, Msg}}
    end;
pgreterr(E) ->
    {error, {unexpected, E}}.


%%
%% 
%%
with_connection(Function) ->
    with_connection(emp, Function).

with_connection(Pool, Function) ->
    psqlcp:with_connection(Pool, Function).
    
%%
%%
%%
with_transaction(Function) ->
    with_transaction(emp, Function).

with_transaction(Pool, Function) ->
    psqlcp:with_transaction(Pool, Function).

%%%
%%% @doc    Выполняет преобразование ответа базы в соответсвии
%%%         со здравым смыслом. Если запрашиваем 1 объект,
%%%         то и разультат должны быть один. Вот это лочигнее
%%%         {ok, [{"name", "Name"}]} =
%%%             dao:one(
%%%                 dao:simple("select name from table where id = 1")
%%%             ).
%%%         чем
%%%             {ok, [[{"name", "Name"}]]} =
%%%                 dao:simple("select name from table where id = 1").
%%%
one({ok, []})         -> {ok, undefined};
one({ok, [Some]})     -> {ok, Some};
one({ok, Several})    -> {ok, Several};
one(Error)            -> Error.

%%% 
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error}
%%%         Может возвращать результат побочного действия.
%%%         Соединение создается само для этого запроса.
%%%
simple(Query)
    when erlang:is_list(Query)
        %orelse  erlang:is_binary(Query)
        orelse  erlang:is_function(Query)
        orelse  (
                    erlang:is_tuple(Query) andalso
                    2 =:= erlang:size(Query) andalso
                    erlang:is_function(erlang:element(1, Query)) andalso
                    erlang:is_function(erlang:element(2, Query))
                ) ->
    dao:with_connection(fun(Con)->eqret(Con, Query)end).

%%%
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error} | [{ok, Result}] | [{error, Error}]
%%%         Возможен множественный запрос без параментров.
%%%         Запросы должны отделяться через `;'.
%%%         Может возвращать результат побочного действия.
%%%         Соединение создается само для этого запроса.
%%%
simple_bulk(Query)
    when erlang:is_list(Query)
        %orelse  erlang:is_binary(Query)
        orelse  erlang:is_function(Query)
        orelse  (
                    erlang:is_tuple(Query) andalso
                    2 =:= erlang:size(Query) andalso
                    erlang:is_function(erlang:element(1, Query)) andalso
                    erlang:is_function(erlang:element(2, Query))
                ) ->
    dao:with_connection(fun(Con)->sqret(Con,Query)end).


%%% 
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error}
%%%         Может возвращать результат побочного действия.
%%%         Соединение создается само для этого запроса.
%%%
simple(Query, Params) ->
    dao:with_connection(fun(Con)->eqret(Con, Query, Params)end).

%%%
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error} 
%%%         Может возвращать результат побочного действия.
%%%         Обертка для функции ?MODULE:equery/2 -> ?MODULE:pgret/1
%%%         Кроме запроса необходимо еще указать соединение.
%%%
eqret(Con, Query)->
    io:format("1 eqret(Con, Query)->~n"),
    S = pgret(equery(Con, Query)),
    io:format("2 eqret(Con, Query)->~n"),
    S.

%%%
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error} | [{ok, Result}] | [{error, Error}]
%%%         Возможен множественный запрос без параментров.
%%%         Запросы должны отделяться через `;'.
%%%         Может возвращать результат побочного действия.
%%%         Обертка для функции ?MODULE:squery/2 -> ?MODULE:pgret/1
%%%         Кроме запроса необходимо еще указать соединение.
%%%
sqret(Con, Query)->
    pgret(squery(Con, Query)).

%%%
%%% @doc    Выполняет простой запрос и возвращает его результат в виде
%%%         {ok, Result} | {error, Error}
%%%         Может возвращать результат побочного действия.
%%%         Обертка для функции ?MODULE:еquery/3 -> ?MODULE:pgret/1
%%%         Кроме запроса необходимо еще указать соединение.
%%%
eqret(Con, Query, Params)->
    io:format("1 eqret(Con, Query, Params)->~n"),
    X = pgret(equery(Con, Query, Params)),
    io:format("2 eqret(Con, Query, Params)->~n"),
    X.

% ---------------------------------------------------------------------------
% %%% @depricated
% %%% @doc  Выполняет простой запрос
% %%%       и возвращает результат побочного действия
% %%%
% simple_ret(Query, Params) ->
%     Pre_result = dao:with_connection(
%             fun(Con) -> equery(Con, Query, Params)
%         end),
%     case dao:pgret(Pre_result) of
%         ok ->
%             Pre_result;
%         Error ->
%             {error, Error}
%     end.
% ---------------------------------------------------------------------------

%%% 
%%% @doc    Обертка для функции ?MODULE:equery/3 c функцией запроса
%%%         Выполныет запрос заданный функцией.
%%%         После самого запроса выполняется Callback
%%%         Callback выполняется отдельным потоком,
%%%             но при этом в случае его падения (в случае ошибки),<<"insert into friend (pers_id, friend_id) values ($pers_id, $friend_id) returning id; ">>

%%%                 текущий процесс тоже упадет
%%%
equery(Con, {Qfunction, Callback}, Params)
        when erlang:is_function(Qfunction)
            andalso erlang:is_function(Callback) ->
    Result = equery(Con, Qfunction, Params),
    spawn_link(
        ?MODULE,
        fcallback,
        [
            Con,
            Callback,
            {
                Qfunction,
                Params,
                Result
            }
        ]
    ),
    Result;

%%% 
%%% @doc    Обертка для функции ?MODULE:equery/3
%%%         Выполныет запрос заданный функцией.
%%%         Функция обязана возвращать строку.
%%%
equery(Con, Qfunction, Params) when erlang:is_function(Qfunction) ->
    equery(Con, fquery(Con, Qfunction, Params), Params);

%%% 
%%% @doc    Обертка для стандвартной функции psqlcp
%%%         Выполныет заданный запрос
%%%         Если параметры функции являются proplist
%%%         то происходит их распарсивание и подстановка согласно
%%%         с буквенными именами в теле запроса
%%%         Сделано это для того, чтобы в случае, если мы будем менять,
%%%         наши запросы, не приходилось высчитывать порядок следования.
%%%         Последнее очень не удобно для запросов с более чем 5 параметров.
%%%
equery(Con, Query, Params) when erlang:is_list(Query);erlang:is_binary(Query) ->
    case is_proplist(Params) of
        true ->
            io:format("FQ = ~p~n", [convert:to_binary(Query)]),
            {NewQuery, Values} = equery_pl(Query, Params),
            io:format("PQ = ~p~n", [convert:to_binary(NewQuery)]),
            io:format("PP = ~p~n", [Params]),
            io:format("PV = ~p~n", [Values]),
            psqlcp:equery(Con, NewQuery, Values);
        _ ->
            io:format("PQ = ~p~n", [convert:to_binary(Query)]),
            io:format("PP = ~p~n", [Params]),
            psqlcp:equery(Con, Query, Params)
    end.

%%% 
%%% @doc    Обертка для функции ?MODULE:equery/2
%%%         Выполныет запрос заданный функцией.
%%%         Функция обязана возвращать строку.
%%%
equery(Con, {Qfunction, Callback})
        when erlang:is_function(Qfunction)
            andalso erlang:is_function(Callback) ->
    Result = equery(Con, Qfunction),
    spawn_link(
        ?MODULE,
        fcallback,
        [
            Con,
            Callback,
            {
                Qfunction,
                Result
            }
        ]
    ),
    Result;

equery(Con, Function) when erlang:is_function(Function) ->
    equery(Con, fquery(Con, Function, []));

equery(Con, Query) when erlang:is_list(Query); erlang:is_binary(Query) ->
    io:format("equery(Con, Query) when erlang:is_list(Query) "),
    psqlcp:equery(Con, Query).


squery(Con, {Qfunction, Callback})
        when erlang:is_function(Qfunction)
            andalso erlang:is_function(Callback) ->
    Result = squery(Con, Qfunction),
    spawn_link(
        ?MODULE,
        fcallback,
        [
            Con,
            Callback,
            {
                Qfunction,
                Result
            }
        ]
    ),
    Result;

squery(Con, Function) when erlang:is_function(Function) ->
    squery(Con, fquery(Con, Function, []));

squery(Con, Query) when erlang:is_list(Query) ->
    psqlcp:squery(Con, Query).


%%% 
%%% @doc    Функция преобразования запроса с буквенными именами к числовым.
%%%         Запрос: dao:equery_pl(
%%%                     "select id from customer where id = $id and uid = $uid",
%%%                     [{"id", 10}, {"uid", 15}]
%%%                 ).
%%%         Вернет: "select id from customer where id = $1 and uid = $2"
%%%
equery_pl(Query, Kvpl) ->
    Rr = convert:to_binary(Query),
    {Newuery, _, Resvalues} =
        lists:foldl(fun
            (<<"$", Item/binary>> , {Prev, Cnt, Values}) ->
                case lists:keyfind(erlang:binary_to_atom(Item, utf8), 1, Kvpl) of
                    {Key, Value} ->
                        {
                            [Prev,equery_variable([{cnt, Cnt}])],
                            Cnt + 1,
                            [Value|Values]
                        };
                    false ->
                        {[Prev, Item], Cnt, Values}
                end;
            (Item, {Prev, Cnt, Values}) ->
                {[Prev, Item], Cnt, Values}
        end, {[], 1, []}, re:split([Query, <<" ">>],"([$].*?)([\\s,;%\\(\\)])")),
    {convert:to_binary(Newuery), lists:reverse(Resvalues)}.

equery_variable(Current)->
    equery_variable(postgres, Current).

equery_variable(postgres, Current)->
    Cnt = proplists:get_value(cnt, Current),
    [<<"$">>, convert:to_list(Cnt)];

equery_variable(mysql, Current)->
    <<"?">>;

equery_variable(Valriant, Current)->
    <<" ">>.


%%%
%%% @doc
%%%     Функция преобразования запроса, основная рабочая часть.
%%%     Параметры разбиваем на {k, v}
%%%         Преобразованное имя ключа $k заменится
%%%             на его порядковый номер в списке.
%%%         Значение (v) отправится в список значений,
%%%             который далее будет использован для стандартного запроса
%%%             тут важно соблюсти порядок следования значений
%%%                 (Values ++[Value]) a не [Value|Values] !!!
%%%     Чтобы не перекомпилевать одинаковые ключи
%%%         результаты подстановки сохраняются
%%%             с использованием мемоизациию.
%%%
%%% TODO:
%%%     Возможно, надо переделать, так, чтобы оно работало и с mysql.
%%%     Изменять не следование переменных в запросе,
%%%     а следование параметров в списке параметров.
%%%
% 
% equery_pl(Query, [], _, Values) ->
%     {Query, Values};
% 
% equery_pl(Query, [{Name, Value}|Rest], Cnt, Values) ->
%     %%%
%     %%% Мемоизациия для вычисления 1 раз.
%     %%%
%     Newquery = equery_construct(Query, Name, Cnt),
%     case Newquery == Query of
%         true ->
%             equery_pl(Query, Rest, Cnt, Values);
%         _ ->
%             equery_pl(Newquery, Rest, Cnt + 1, lists:append(Values, [Value]))
%     end;
% 
% equery_pl(Query, List, _, _) when erlang:is_list(List) ->
%     {Query, List}.
% 
% %%%
% %%% @doc
% %%%     Функция преобразования запроса,
% %%%     Подстановка.
% %%%
% equery_construct(Query, Name, Cnt) ->
%     %%% 
%     %%% Мемоизациия для вычисления 1 раз.
%     %%% Многие паттерны встречаются достаточно часто,
%     %%%     более чем в одном запросе
%     %%% Например id, name ...
%     %%%
%     re:replace(
%         Query,
%         equery_construct_re(Name),
%         ?SVAR ++ convert:to_list(Cnt),
%         [global, {return,list}]
%     ).
% 
% %%%
% %%% @doc
% %%%     Функция преобразования запроса,
% %%%     Формирование паттерна для подстановки
% %%%
% equery_construct_re(Name)->
%     {ok, Cre} =
%         re:compile(
%             "[" ++
%                 ?SVAR ++
%             "]" ++
%             convert:to_list(Name)
%         ),
%     Cre.

%%% 
%%% @doc
%%%     Конструирует запрос, заданный функцией.
%%%     Кеширует резльтат выполнения функции.
%%%     !ВАЖНО: что функция вызывается до выполнения самого запроса
%%%
fquery(Con, Qfunction, Param)
        when erlang:is_function(Qfunction, 2) ->
    memo:lsave(Qfunction, [Con, Param]);

fquery(Con, Qfunction, _)
        when erlang:is_function(Qfunction, 1) ->
    memo:lsave(Qfunction, [Con]);

fquery(_, Qfunction, _)
        when erlang:is_function(Qfunction, 0) ->
    memo:lsave(Qfunction, []).


%%% @doc
%%%     Конструирует запрос, заданный функцией.
%%%     Кеширует резльтат выполнения функции.
%%%     !ВАЖНО: что функция вызывается после выполнения самого запроса
%%%
fcallback(Con, Callback, {Qfunction, Params, Result})
        when erlang:is_function(Callback, 4) ->
    memo:lsave(Callback, [Con, Qfunction, Params, Result]);

fcallback(Con, Callback, {Qfunction, Params, Result})
        when erlang:is_function(Callback, 2) ->
    memo:lsave(Callback, [Con, {Qfunction, Params, Result}]);

fcallback(Con, Callback, _)
        when erlang:is_function(Callback, 1) ->
    memo:lsave(Callback, [Con]);

fcallback(_, Callback, _)
        when erlang:is_function(Callback, 0) ->
    memo:lsave(Callback, []).

%%% -------------------------------------------------------------------------
%%% Функциии тестирования
%%% -------------------------------------------------------------------------

is_proplist([]) ->
    true;
is_proplist([{K,_}|R]) when is_atom(K) ->
    %%% {key, _}
    is_proplist(R);
is_proplist([{K,_}|R]) when is_list(K) ->
    %%% {"key", _}
    is_proplist(R);

is_proplist(_) ->
    false.

test()->

    ok.


test(speed)->
    ok.
