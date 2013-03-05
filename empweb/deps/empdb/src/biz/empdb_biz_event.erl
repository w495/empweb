%% @file    empdb_biz_event.erl
%%          Описание бизнес логики работы с фотографиями.
%%          Фотография это просто документ.
%% 
-module(empdb_biz_event).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").


%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Блоги
%%
-export([
    count/1,
    get/1,
    get/2,
    create/1,
    delete/1,
    update/1
]).

create(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_event:create(Con, Params)
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_event:update(Con, Params)
    end).

count(Params)->
    
    empdb_dao:with_connection(fun(Con)->
        case empdb_dao_event:count(Con, [{isdeleted, false}|Params]) of
            {ok, [{Allcountpl}]}->
                {ok, [{Exilecountpl}]} =
                    empdb_dao_event:count(Con, [
                        {isdeleted,         false},
                        {eventobj_alias,    exile},
                        {eventact_alias,    create},
                        {eventtype_alias,   create_exile}
                        |Params
                    ]),
                    Exilecount = proplists:get_value(count, Exilecountpl, 0),

                    spawn_link(fun()->
                        %% Ключевой момент: без spawn_link код ниже может
                        %% привести к блокировкам, а так,
                        %% он выполняется независимо.
                        empdb_dao:with_transaction(emp, fun(Conupdate) ->
                            %%
                            %% Ставим пользователю статус online
                            %%
                            {ok, _} =
                                empdb_dao_pers:update(Conupdate, [
                                    {pstatus_alias, online},
                                    {id, proplists:get_value(owner_id, Params)}
                                ])
                        end)
                    end),

                {ok, [{[{exilecount, Exilecount}|Allcountpl]}]};
            Allcountelse ->
                Allcountelse
        end
    end).


    
get(Params)->
    empdb_biz:nviewsupm(?MODULE, [Params]),
    empdb_dao:with_connection(fun(Con)->
        case empdb_dao_event:get(Con, [
            {order, {desc, created}},
            {isdeleted, false}
            |Params
        ]) of
            {ok, Ok} ->
                delete_viewed(),
                {ok, Ok};
            Else ->
                Else
        end
    end).

get(Params, Fileds)->
    empdb_biz:nviewsupm(?MODULE, [Params]),
    empdb_dao:with_connection(fun(Con)->
        case empdb_dao_event:get(Con, [
            {order, {desc, created}},
            {isdeleted, false}
            |Params
        ], Fileds) of
            {ok, Ok} ->
                delete_viewed(),
                {ok, Ok};
            Else ->
                Else
        end
    end).

delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_event:update(Con, [
            {filter, [{isdeleted, false}|Params]},
            {values, [{isdeleted, true}]}
        ])
    end).

is_owner(Uid, Oid)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_event:is_owner(Con, Uid, Oid)
    end).


delete_viewed() ->
    erlang:spawn_link(fun()->
        empdb_dao:with_connection(fun(Con)->
            empdb_dao_event:update(Con, [
                {filter, [
                    {isnews,    false},
                    {isdeleted, false},
                    {nviews,    {gt, 0}}
                ]},
                {values, [
                    {isdeleted, true}
                ]}
            ])
        end)
    end).
