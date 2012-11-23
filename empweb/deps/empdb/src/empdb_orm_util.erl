-module(empdb_orm_util).


-export([
    current_all_fields/2,
    current_select_fields/2
]).


current_select_fields(Filtername, Op) ->
    Filternamestr = empdb_convert:to_list(Filtername),
    case lists:member($., empdb_convert:to_list(Filtername)) of
        true ->
            Filtername;
        _ ->
            case lists:foldl(
                fun ({Tab, _}, [])->
                        case
                            lists:member(
                                Filtername,
                                table_options(
                                    {table, fields, select},
                                    Tab
                                )
                            )
                        of
                            true ->
                                empdb_convert:to_atom(
                                    empdb_convert:to_list(
                                        table_options({table, name},Tab)
                                    )
                                    ++ "." ++
                                    Filternamestr
                                );
                            _ ->
                                []
                        end;
                    ({Tab, _}, Res) ->
                        Res
                end, [], Op
            ) of
                [] ->
                    Filtername;
                Newfiltername ->
                    Newfiltername
            end
    end.

    
current_all_fields(Current_all_fields_, Op) ->
    lists:map(
        fun ({Filtername, Filterval}) when is_atom(Filtername) ->
                Filternamestr = empdb_convert:to_list(Filtername),
                case lists:member($., empdb_convert:to_list(Filtername)) of
                    true ->
                        {Filtername, Filterval};
                    _ ->

                        case lists:foldl(
                            fun ({Tab, _}, [])->
                                    case
                                        lists:member(
                                            Filtername,
                                            table_options(
                                                {table, fields, all},
                                                Tab
                                            )
                                        )
                                    of
                                        true ->
                                            case lists:member(Filtername, table_expressions()) of
                                                true ->
                                                    {Filtername, current_all_fields(Filterval, Op)};
                                                _ ->
                                                    {   empdb_convert:to_atom(
                                                            empdb_convert:to_list(
                                                                table_options({table, name},Tab)
                                                            )
                                                            ++ "." ++
                                                            Filternamestr
                                                        ),
                                                        Filterval
                                                    }
                                            end;
                                        _ ->
                                            []
                                    end;
                                ({Tab, _}, Res) ->
                                    Res
                            end,
                            [],
                            Op
                        ) of
                            [] ->
                                {Filtername, Filterval};
                            {Newfiltername, Newfilterval} ->
                                {Newfiltername, Newfilterval}
                        end
                end;
            ({Filtername, Filterval})  ->
                {Filtername, Filterval}
        end,
        Current_all_fields_
    ).

table_expressions()->
    [
        'or',
        'and',
        'not'
    ].

table_options(Current) when erlang:is_atom(Current) ->
    [
        {   {table, name},
            Current:table(name)
        },
        {   {table, fields, all},
            Current:table({fields, all})
        },
        {   {table, fields, select},
            Current:table({fields, select})
        },
        {   {table, fields, update},
            Current:table({fields, update})
        },
        {   {table, fields, insert},
            Current:table({fields, insert})
        },
        {   {table, fields, insert, required},
            Current:table({fields, insert, required})
        }
    ];

table_options(Current) ->
    Current.

table_options({table, fields, all},      Current) ->
    [   'and', 'or'
        | proplists:get_value({table, fields, all}, Current, [])
    ];

table_options(Oname,      Current) ->
    proplists:get_value(Oname, Current).

