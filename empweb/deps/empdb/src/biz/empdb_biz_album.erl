%% @file'empdb_biz_album.erl'%%          Описание бизнес логики работы с альбомами.
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

        %case proplists:get_value(
            %contype_alias,
            %Params,
            %proplists:get_value(
                %contype_alias,
                %proplists:get_value(filter, Params, []),
                %null
            %)
        %) of
            %adult_only ->

                %{ok,[{Servicepl}]} =
                    %empdb_dao_service:get( Con, [
                            %{alias, create_adult_only_album_price},
                            %{fields, [price]},
                            %{limit, 1}
                    %]),

                %Price = proplists:get_value(price, Servicepl),

                %{ok, [{Mbownerpl}]} =
                    %empdb_dao_pers:get(Con, [
                        %{id,
                            %proplists:get_value(
                                %owner_id,
                                %Params,
                                %null
                            %)
                        %},
                        %{fields, [
                            %id,
                            %money
                        %]},
                        %{limit, 1}
                    %]),

                %Owner_id = proplists:get_value(id,   Mbownerpl),
                %Money = proplists:get_value(money,   Mbownerpl),

                %case Money >= Price of

                    %false ->
                        %{error, {not_enough_money, {[
                            %{money, Money},
                            %{price, Price}
                        %]}}};
                %end;
            %_ ->


        %case empdb_dao_roomlot:get(Con,[
            %{   filter,
                %[
                    %{isdeleted, false}
                    %|proplists:get_value(filter, Params, [
                        %{room_id, proplists:get_value(room_id, Params)}
                    %])
                %]
            %}
        %]) of
            %{ok, []} ->
                %empdb_dao_roomlot:create(Con, Params);
            %{ok, _} ->
                %{error, {not_uniq_roomlot, {[
                    %{room_id,
                        %proplists:get_value(room_id, Params,
                            %proplists:get_value(room_id,
                                %proplists:get_value(filter, Params, [])
                            %)
                        %)
                    %}
                %]}}};
            %Elseget ->
                %Elseget
        %end

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
                {order, {desc, created}},
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
                                        {ok, [{Defaultimagepl}]} =
                                            empdb_biz_file:get_system_picture(
                                                Con,
                                                [
                                                    {image_width,
                                                        proplists:get_value(
                                                            image_width,
                                                            Params,
                                                            null
                                                        )
                                                    },
                                                    {image_height,
                                                        proplists:get_value(
                                                            image_height,
                                                            Params,
                                                            null
                                                        )
                                                    },
                                                    {limit, 1},
                                                    {alias, default_album_image}
                                                ]
                                            ),

                                        case empdb_dao_photo:get(Con, [
                                            {isdeleted, false},
                                            {order, {desc, 'photo.created'}},
                                            {parent_id, Id},
                                            {image_width,
                                                proplists:get_value(
                                                    image_width,
                                                    Params,
                                                    null
                                                )
                                            },
                                            {image_height,
                                                proplists:get_value(
                                                    image_height,
                                                    Params,
                                                    null
                                                )
                                            },
                                            {limit, 1}
                                        ]) of
                                            {ok, []} ->
                                                {[
                                                    {path,
                                                        proplists:get_value(
                                                            path,
                                                            Defaultimagepl,
                                                            null
                                                        )
                                                    }
                                                    |Albumpl
                                                ]};
                                            {ok, [{Photopl}]} ->
                                                Path = proplists:get_value(
                                                    path,
                                                    Photopl,
                                                    proplists:get_value(
                                                        path,
                                                        Defaultimagepl,
                                                        null
                                                    )
                                                ),
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


