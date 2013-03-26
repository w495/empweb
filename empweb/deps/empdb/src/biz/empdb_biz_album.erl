%% @file    empdb_biz_album.erl
%%          Описание бизнес логики работы с альбомами.
%%          Альбом это просто документ.
%% 
-module(empdb_biz_album).

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

-export([
    get/1,
    get/2,
    create/1,
    delete/1,
    update/1
]).

create(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_album:create(Con, Params)
    end).

update(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_album:update(Con, Params)
    end).

get(Params)->
    empdb_dao:with_connection(fun(Con)->
        empdb_dao_album:get_adds(Con,
            case empdb_dao_album:get(Con, [
                {isdeleted, false}
                |Params
            ]) of
                {ok, Albums} ->
                    {ok,
                        lists:map(
                            fun({Albumpl})->
                                case proplists:get_value(id, Albumpl) of
                                    undefined ->
                                        {[{path, null}|Albumpl]};
                                    Id ->
                                        case empdb_dao_photo:get(Con, [
                                            {isdeleted, false},
                                            {order, {desc, 'photo.created'}},
                                            {parent_id, Id},
                                            {image_width,
                                                proplists:get_value(image_width, Params, null)
                                            },
                                            {image_height,
                                                proplists:get_value(image_height, Params, null)
                                            },
                                            {limit, 1},
                                            {fields, [path]}
                                        ]) of
                                            {ok, []} ->
                                                {[{path, null}|Albumpl]};
                                            {ok, [Photopl]} ->
                                                Path = proplists:get_value(path, Photopl, null),
                                                {[{path, Path}|Albumpl]};
                                            {Eclassp, Ereasonp} ->
                                                {Eclassp, Ereasonp}
                                        end
                                end
                            end,
                            Albums
                        )
                    };
                {Eclassa, Ereasona} ->
                    {Eclassa, Ereasona}
            end
        )
    end).

get(Params, Fileds)->
   ?MODULE:get([{fileds, Fileds}|Params]).

delete(Params)->
    empdb_dao:with_transaction(fun(Con)->
        empdb_dao_album:update(Con, [{isdeleted, true}|Params])
    end).


