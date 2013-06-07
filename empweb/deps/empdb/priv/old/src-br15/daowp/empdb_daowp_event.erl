%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_daowp_event).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
    feedfriends/1,
    feedfriends/2,
    feedfriends/3
]).


%
% -define\(CREATE_event_PRICE, 1.0).
%

-record(ffo, {
    uniq = false
}).

feedfriends(Param) ->
    feedfriends(Param, []).

feedfriends(Param, Opts) when  erlang:is_list(Param)->
    spawn_link(fun()->
        empdb_dao:with_connection(emp, fun(Con)->
            feedfriends(Con, Param, Opts)
        end)
    end);

feedfriends(Con, Param) ->
    feedfriends(Con, Param, []).

feedfriends(Con, Param, Opts) ->
    Ffo = #ffo{
        uniq = proplists:get_value(uniq, Opts, false)
    },
    Pers_id = proplists:get_value(pers_id, Param),
    %% Выбираем подписчиковs
    case empdb_dao_friend:get(Con, [
        {friend_id, Pers_id},
        {friendtype_alias, friend}
    ]) of
        {ok, []} ->
            ok;
        {ok, Persobjs} ->
            %% Выбирали подписчиков
            %% Отсылаем им всем сообщения
            lists:map(
                fun({Perspl})->
                    do_feedfriends(
                        %% соединение
                            Con,
                        %% получатель
                            proplists:get_value(pers_id, Perspl),
                        %% о ком информация
                            Pers_id,
                        %% доп. параметры
                            Param,
                        %% опции
                            Ffo
                    )
                end,
                Persobjs
            );
        Perselse ->
            Perselse
    end.


do_feedfriends(
    Con,        %% соединение
    Owner_id,   %% получатель
    Pers_id,    %% о ком информация
    Param,      %% доп. параметры
    #ffo{uniq=true} = Ffo   %% опции
) ->
    case empdb_dao_event:get(Con, [
        {owner_id,        Owner_id},
        {pers_id,         Pers_id}
        |Param
    ]) of
        {ok, []} ->
            do_feedfriends(
                Con,
                Owner_id,
                Pers_id,
                Param,
                Ffo#ffo{uniq=false}
            );
        Eventelse ->
            Eventelse
    end;

do_feedfriends(
    Con,        %% соединение
    Owner_id,   %% получатель
    Pers_id,    %% о ком информация
    Param,      %% доп. параметры
    _           %% опции
) ->
    empdb_dao_event:create(Con, [
        {owner_id,        Owner_id},
        {pers_id,         Pers_id}
        |Param
    ]).




%%
%% Local Functions
%%

