%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_daowp_room).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
    get/2,
    get/3
]).


%
% -define\(CREATE_ROOM_PRICE, 1.0).
%

get(Con, Params)->
    get_adds(Con,
        empdb_dao_room:get(Con, [
            {isdeleted, false}
            |Params
        ] ++ [
            {order, {desc, 'doc.head'}}
        ]),
        Params
    ).

get(Con, Params, Fields)->
    get_adds(Con,
        empdb_dao_room:get(Con, [
            {isdeleted, false}
            |Params
        ] ++ [
            {order, {desc,'doc.head'}}
        ], Fields),
        [{fields, Fields}| Params]
    ).


get_adds(Con, {ok, Rooms}, Params) ->
    Fields = proplists:get_value(fields, Params, []),
    {ok,
        lists:map(
            fun({Roompl})->


                {ok,[{[{count,Nops}]}]} =
                    empdb_dao_pers:count(Con, [
                        {isdeleted, false},
                        {pstatus_alias, online},
                        {live_room_id, proplists:get_value(id, Roompl)}
                    ]),

                {ok, Topiclist} =
                    empdb_dao_room:get_room_topic(Con, [
                        {isdeleted, false},
                        {room_id,proplists:get_value(id, Roompl)}
                    ]),


                Nroompl = [{nops, Nops}|Roompl],

                Room_id = proplists:get_value(id, Roompl, null),
                Old_back_file_id =
                    proplists:get_value(back_file_id, Roompl),

                {Backfileid, Backfilepath} =
                    case empdb_dao_thingbuy:get(Con, [
                        {room_id, Room_id},
                        {file_id, proplists:get_value(back_file_id, Roompl)},
                        {limit, 1},
                        {fields, [file_id]}
                    ]) of
                        {ok, []} ->
                            filepath(
                                Con,
                                [
                                    {back_file_id, null}
                                    |proplists:delete(back_file_id, Nroompl)
                                ],
                                back_file_id,
                                default_room_background
                            );
                        _ ->
                            filepath(Con, Nroompl, back_file_id, null)
                            %filepath(
                                %Con,
                                %[
                                    %{back_file_id, null}
                                    %|proplists:delete(back_file_id, Nroompl)
                                %],
                                %back_file_id,
                                %default_room_background
                            %)
                    end,


                {Wallfileid, Wallfilepath} = filepath(Con, Nroompl, wall_file_id),
                {Flagfileid, Flagfilepath} = filepath(Con, Nroompl, flag_file_id),
                {Armsfileid, Armsfilepath} = filepath(Con, Nroompl, arms_file_id),

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
                        proplists:delete(
                            back_file_id,
                            Nroompl
                        ),
                        [
                            {topic_list,        Topiclist},
                            {back_file_id,      Backfileid},
                            {back_file_path,    Backfilepath},
                            {wall_file_path,    Wallfilepath},
                            {flag_file_path,    Flagfilepath},
                            {arms_file_path,    Armsfilepath}
                        ]
                    )
                }
            end,
            Rooms
        )
    };

get_adds(_con, Else, _params) ->
    Else.


filepath(Con, Roompl, Idfield) ->
    filepath(Con, Roompl, Idfield, null).

filepath(Con, Roompl, Idfield, Alias) ->
    case empdb_dao:get([
        {empdb_dao_file, id},
        {empdb_dao_fileinfo, file_id}
    ],Con,[
        {'or', [
            {'and', [
                {'file.id',     proplists:get_value(Idfield, Roompl, null)},
                {'file.alias',  null}
            ]},
            {'and', [
                {'file.alias', {'neq', null}},
                {'file.alias', Alias}
            ]}
        ]},
        %{'file.id',     proplists:get_value(Idfield, Roompl, null)},
        {fileinfotype_alias,
            download},
        {image_width,  null},
        {image_height, null},
        {limit, 1},
        {fields, [
            {as, {'fileinfo.path', fileinfopath}},
            {as, {'fileinfo.dir',  fileinfodir}},
            file_id
        ]}
    ]) of
        {ok, []} ->
            {   proplists:get_value(file_id, Roompl),
                null
            };
        {ok, [{Filepl}]} ->
            {   proplists:get_value(file_id, Filepl),
                <<  (proplists:get_value(fileinfodir, Filepl))/binary,
                    (proplists:get_value(fileinfopath, Filepl))/binary
                >>
            }
    end.


%%
%% Local Functions
%%

