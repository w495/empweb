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
            {order, {desc, doc.created}}
        ]),
        Params
    ).

get(Con, Params, Fields)->
    get_adds(Con,
        empdb_dao_room:get(Con, [
            {isdeleted, false}
            |Params
        ] ++ [
            {order, {desc, doc.created}}
        ], Fields),
        [{fields, Fields}| Params]
    ).

        
get_adds(Con, {ok, Rooms}, Params) ->
    Fields = proplists:get_value(fields, Params, []),
    {ok,
        lists:map(
            fun({Roompl})->
                {ok, Topiclist} =
                    empdb_dao_room:get_room_topic(Con, [
                        {isdeleted, false},
                        {room_id,proplists:get_value(id, Roompl)}
                    ]),

                Backfilepath = filepath(Con, Roompl, back_file_id),
                Wallfilepath = filepath(Con, Roompl, wall_file_id),
                Flagfilepath = filepath(Con, Roompl, flag_file_id),
                Armsfilepath = filepath(Con, Roompl, arms_file_id),

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
                        Roompl,
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
            Rooms
        )
    };

get_adds(_con, Else, _params) ->
    Else.

filepath(Con, Roompl, Idfield) ->
    case empdb_dao:get([
        {empdb_dao_file, id},
        {empdb_dao_fileinfo, file_id}
    ],Con,[
        {file.id,
            proplists:get_value(Idfield, Roompl)},
        {fileinfotype_alias,
            download},
        {limit, 1},
        {fields, [
            {as, {fileinfo.path, fileinfopath}},
            {as, {fileinfo.dir,  fileinfodir}}
        ]}
    ]) of
        {ok, []} ->
            null;
        {ok, [{List}]} ->
            <<  (proplists:get_value(fileinfodir, List))/binary,
                (proplists:get_value(fileinfopath, List))/binary
            >>
    end.


%%
%% Local Functions
%%

