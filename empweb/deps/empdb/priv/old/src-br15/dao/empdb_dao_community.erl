%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_dao_community).
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
    count/2,
    get_community_topic/2,
    get_community_topic/3,
    add_community_topic/2,
    delete_community_topic/2,
    get/2,
    get/3
]).



%%
%% Exported Functions
%%
-export([
    update_communitytype/2,
    create_communitytype/2,
    get_communitytype/2,
    get_communitytype/3
]).


%%
%% Темы чата комнаты.
%%
-export([
    update_topic/2,
    create_topic/2,
    get_topic/2,
    get_topic/3
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
        doc_id,
        back_file_id,
        back_path,
        wall_file_id,
        wall_path,
        flag_file_id,
        flag_path,
        arms_file_id,
        arms_path,
        nmembs,
        ncands,
        communitytype_id,
        communitytype_alias,
        read_gte_authority_id,
        read_gte_authority_level,
        read_gte_authority_alias,
        cands_gte_authority_id,
        cands_gte_authority_alias,
        cands_gte_authority_level,
        slogan,
        treas,
        isclosed,
        fee
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
    community.

table()->
    table(name).

get_community_topic(Con, What) ->
    empdb_dao:get([
%         {empdb_dao_doc, id},
%         {empdb_dao_community, doc_id},
%         {community2topic(), {community_id, topic_id}},
        {community2topic(), topic_id },
        {topic(),      id       }
     ], Con, What).

get_community_topic(Con, What, Fields)->
    empdb_dao:get([
%         {empdb_dao_doc, id},
%         {empdb_dao_community, doc_id},
%         {community2topic(), {community_id, topic_id}},
        {community2topic(),  topic_id},
        {topic(),       id      }
    ], Con, What, Fields).

% add_community_topic(Con, What)->
%     empdb_dao:insert(community2topic(), Con, What).
%
%
% del_community_topic(Con, What)->
%     empdb_dao:delete(community2topic(), Con, What).



add_community_topic(Con, Proplist)->
    case empdb_dao:pgret(
        empdb_dao:equery(Con,
            <<"insert into community2topic (topic_id, community_id) "
                "values ($topic_id, $community_id) "
                "returning id">>,
            Proplist
        )
    ) of
        {error,{not_unique,<<"topic_id_community_id_many">>}} ->
            {error, {not_unique, [topic_id, community_id]}};
        Res ->
            Res
    end.

delete_community_topic(Con, Proplist)->
    case empdb_dao:pgret(
        empdb_dao:equery(Con,
            <<"delete from community2topic where "
            " topic_id=$topic_id and community_id=$community_id returning id">>,
            Proplist
        )
    ) of
        {ok, 0} ->
            {error, not_exists};
        Res ->
            Res
    end.

count(Con, What) ->
    empdb_dao_doc:count(?MODULE, Con, What).

get(Con, What) ->
    case proplists:get_value(topic_id, What) of
        undefined ->
            empdb_dao_doc:get(?MODULE, Con, What);
        Topic_id ->
            empdb_dao:get([
                {empdb_dao_doc, id},
                {empdb_dao_community, doc_id},
                {community2topic(), community_id}
            ], Con, What)
    end
    .

get(Con, What, Fields)->
    case proplists:get_value(topic_id, What) of
        undefined ->
            empdb_dao_doc:get(?MODULE, Con, What, Fields);
        Topic_id ->
            empdb_dao:get([
                {empdb_dao_doc, id},
                {empdb_dao_community, doc_id},
                {community2topic(), community_id}
            ], Con, What, Fields)
    end.

create(Con, Proplist)->
    empdb_dao_doc:create(?MODULE, Con, Proplist).

update(Con, Proplist)->
    empdb_dao_doc:update(?MODULE, Con, Proplist).

is_owner(Con, Owner_id, Obj_id) ->
    empdb_dao_doc:is_owner(Con, Owner_id, Obj_id).



get_communitytype(Con, What) ->
    get_communitytype(Con, What, []).

get_communitytype(Con, What, Fields)->
    empdb_dao:get(communitytype(), Con, What, Fields).

create_communitytype(Con, Proplist)->
    empdb_dao:get(communitytype(), Con, Proplist).

update_communitytype(Con, Proplist)->
    empdb_dao:get(communitytype(), Con, Proplist).



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Темы сообщества
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_topic(Con, What) ->
    empdb_dao:get(topic(), Con, What).

get_topic(Con, What, Fields)->
    empdb_dao:get(topic(), Con, What, Fields).

create_topic(Con, Proplist)->
    empdb_dao:create(topic(), Con, Proplist).

update_topic(Con, Proplist)->
    empdb_dao:update(topic(), Con, Proplist).
    

%%
%% Local Functions
%%


communitytype() ->
    [
        {{table, name},                       communitytype},
        {{table, fields, all},                [id, name_ti, alias, isdeleted]},
        {{table, fields, select},             [id, name_ti, alias]},
        {{table, fields, insert},             [name_ti, alias]},
        {{table, fields, update},             [name_ti, alias]},
        {{table, fields, insert, required},   [alias]}
    ].





%%
%% @doc Описывает темы чата комнаты.
%% Сами по себе темы представлют из себя дерево:
%% -------------------------------------------------
%%  [Все темы]
%%      |-[Автомобили]
%%          |-[Хорошие]
%%              |-[Чайка]
%%              |-[Уазик]
%%          |-[Плохие]
%%              |-[Калина]
%%              |-[Запорожец]
%%      ...
%% -------------------------------------------------
%%
topic() ->
    [
        %% Имя таблицы.
        {{table, name},                       topic},
        %% Список всех полей.
        {{table, fields, all},                [
            id,
            alias,
            name_ti,    %% имя на нескольких языках.
            descr_ti,   %% описание на нескольких языках.
            parent_id,  %% ссылка на родительскую тему
            nchildren,  %% количество детей
            nnodes,     %% количество потомков
            nchildtargets,
            isdeleted
        ]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [
            id,
            name_ti,
            descr_ti,
            parent_id,
            alias,
            nchildtargets,
            nroomtargets,
            ncommunitytargets,
            nchildren,
            nnodes
        ]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [
            name_ti, descr_ti, parent_id, alias
        ]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [
            name_ti, 
            descr_ti, 
            parent_id, 
            alias, 
            ncommunitytargets,
            nchildtargets
        ]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [
            name_ti, descr_ti, parent_id, alias
        ]}
    ].


community2topic() ->
   [
        %% Имя таблицы.
        {{table, name},                       community2topic},
        %% Список всех полей.
        {{table, fields, all},                [
            topic_id,
            community_id,
            isdeleted
        ]},
        %% Список полей по которым можно проводить выборку.
        {{table, fields, select},             [
            topic_id,
            community_id,
            isdeleted
        ]},
        %% Список полей таблицы для создания.
        {{table, fields, insert},             [
            topic_id,
            community_id,
            isdeleted
        ]},
        %% Список полей таблицы для обновления.
        {{table, fields, update},             [
            topic_id,
            community_id,
            isdeleted
        ]},
        %% Cписок обязательных полей таблицы для создания.
        {{table, fields, insert, required},   [
            topic_id,
            community_id,
            isdeleted
        ]}
    ].
