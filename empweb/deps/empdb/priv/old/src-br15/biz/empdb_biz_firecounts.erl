%% @file    empdb_biz_event.erl
%%          Описание бизнес логики работы с фотографиями.
%%          Фотография это просто документ.
%%
-module(empdb_biz_firecounts).

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
    get/1
]).


get(Params)->
    empdb_dao:with_connection(fun(Con)->

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

        case {
            empdb_dao_event:count(Con, [{isdeleted, false}|Params]),
            empdb_dao_exile:count(Con, [{isdeleted, false}|Params])
        } of
            {
                {ok, [{Event}]},
                {ok, [{Exile}]}
            } ->
                {ok, [{[
                    {event_count,   proplists:get_value(count, Event, null)},
                    {exile_count,   proplists:get_value(count, Exile, null)},
                    {'now',         empdb_convert:datetime2int(erlang:universaltime())}
                ]}]};
            {
                Error,
                _
            } ->
                Error
        end
    end).


