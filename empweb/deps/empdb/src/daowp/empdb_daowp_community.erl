%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_daowp_community).

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
% -define\(CREATE_Community_PRICE, 1.0).
%

get(Con, Params)->
    get_adds(Con,
        empdb_dao_community:get(Con, [
            {isdeleted, false}
            |Params
        ] ++ [
            {order, {desc, 'doc.created'}}
        ]),
        Params
    ).

get(Con, Params, Fields)->
    get_adds(Con,
        empdb_dao_community:get(Con, [
            {isdeleted, false}
            |Params
        ] ++ [
            {order, {desc,'doc.created'}}
        ], Fields),
        [{fields, Fields}| Params]
    ).


get_adds(Con, {ok, Communitys}, Params) ->
    Fields = proplists:get_value(fields, Params, []),
    {ok,
        lists:map(
            fun({Communitypl})->


                {ok,[{[{count,Nops}]}]} =
                    empdb_dao_pers:count(Con, [
                        {isdeleted, false},
                        {pstatus_alias, online},
                        {live_community_id, proplists:get_value(id, Communitypl)}
                    ]),

                {ok, Topiclist} =
                    empdb_dao_community:get_community_topic(Con, [
                        {isdeleted, false},
                        {community_id,proplists:get_value(id, Communitypl)}
                    ]),


                Ncommunitypl = [{nops, Nops}|Communitypl],

                Community_id = proplists:get_value(id, Communitypl, null),
                Old_back_file_id =
                    proplists:get_value(back_file_id, Communitypl),

                {Backfileid, Backfilepath} =
                    case empdb_dao_thingbuy:get(Con, [
                        {community_id, Community_id},
                        {file_id, proplists:get_value(back_file_id, Communitypl)},
                        {limit, 1},
                        {fields, [file_id]}
                    ]) of
                        {ok, []} ->
                            filepath(
                                Con,
                                [
                                    {back_file_id, null}
                                    |proplists:delete(back_file_id, Ncommunitypl)
                                ],
                                back_file_id,
                                default_cdoc_background
                            );
                        _ ->
                            filepath(Con, Ncommunitypl, back_file_id, null)
                            %filepath(
                                %Con,
                                %[
                                    %{back_file_id, null}
                                    %|proplists:delete(back_file_id, Ncommunitypl)
                                %],
                                %back_file_id,
                                %default_community_background
                            %)
                    end,


                {Wallfileid, Wallfilepath} = filepath(Con, Ncommunitypl, wall_file_id),
                {Flagfileid, Flagfilepath} = filepath(Con, Ncommunitypl, flag_file_id),
                {Armsfileid, Armsfilepath} = filepath(Con, Ncommunitypl, arms_file_id),

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
                            Ncommunitypl
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
            Communitys
        )
    };

get_adds(_con, Else, _params) ->
    Else.


filepath(Con, Communitypl, Idfield) ->
    filepath(Con, Communitypl, Idfield, null).

filepath(Con, Communitypl, Idfield, Alias) ->
    case empdb_dao:get([
        {empdb_dao_file, id},
        {empdb_dao_fileinfo, file_id}
    ],Con,[
        {'or', [
            {'and', [
                {'file.id',     proplists:get_value(Idfield, Communitypl, null)},
                {'file.alias',  null}
            ]},
            {'and', [
                {'file.alias', {'neq', null}},
                {'file.alias', Alias}
            ]}
        ]},
        %{'file.id',     proplists:get_value(Idfield, Communitypl, null)},
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
            {   proplists:get_value(file_id, Communitypl),
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

