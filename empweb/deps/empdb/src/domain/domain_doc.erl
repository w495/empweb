%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(domain_doc).

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

%% --------------------------------------------------------------------------
%% УТИЛИТАРНЫЕ ОБЪЕКТЫ
%% --------------------------------------------------------------------------

%%
%% Тип разрешения: не рассмотрен, запрещена, разрешена
%%
-export([
    get_oktype/1,
    get_oktype/2,
    create_oktype/1,
    update_oktype/1
]).

%%
%% Тип документа: Блог, коммент к блогу, галерея,
%%      фото, коммент к фото, attach descr.
%%
-export([
    update_doctype/1,
    create_doctype/1,
    get_doctype/1,
    get_doctype/2
]).

%%
%% Типы контента: обычный, эротический
%%
-export([
    update_contype/1,
    create_contype/1,
    get_contype/1,
    get_contype/2
]).


%%
%% Тип доступа к контенту контента (блога и галереи):
%%  приватный, дружеский, открытый.
%%
-export([
    update_acctype/1,
    create_acctype/1,
    get_acctype/1,
    get_acctype/2
]).

%%
%% Типы чат-комнат. (страна, тюрьма, ад, рай).
%%
-export([
    get_roomtype/1,
    get_roomtype/2,
    create_roomtype/1,
    update_roomtype/1
]).

%%
%% Список языков чата.
%%
-export([
    get_chatlang/1,
    get_chatlang/2,
    create_chatlang/1,
    update_chatlang/1
]).

%%
%% Список режимов комнаты: дектатура, демократия и пр.
%%
-export([
    get_regimen/1,
    get_regimen/2,
    create_regimen/1,
    update_regimen/1
]).

%%
%% Дерево тем комнаты.
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
-export([
    get_topic/1,
    get_topic/2,
    create_topic/1,
    update_topic/1
]).


%%
%% Типы сообществ (обычные, тайные)
%%
-export([
    get_communitytype/1,
    get_communitytype/2,
    create_communitytype/1,
    update_communitytype/1
]).

%%
%% Типы сообщений
%%
-export([
    get_messagetype/1,
    get_messagetype/2,
    create_messagetype/1,
    update_messagetype/1
]).


%% --------------------------------------------------------------------------
%% ЗНАЧИМЫЕ ОБЪЕКТЫ
%% --------------------------------------------------------------------------

%%
%% Блоги
%%
-export([
    get_blog/1,
    get_blog/2,
    create_blog/1,
    update_blog/1,
    delete_blog/1
]).

%%
%% Посты 
%%
-export([
    get_post/1,
    get_post/2,
    create_post/1,
    update_post/1,
    delete_post/1
]).


%%
%% Коменты
%%
-export([
    get_comment/1,
    get_comment/2,
    create_comment/1,
    update_comment/1,
    delete_comment/1
]).


%%
%% Чат-комнаты (комнаты)
%%
-export([
    get_room/1,
    get_room/2,
    create_room/1,
    update_room/1,
    delete_room/1
]).

%%
%% Сообщества.
%%
-export([
    get_community/1,
    get_community/2,
    create_community/1,
    update_community/1,
    delete_community/1
]).

%%
%% Сообщения
%%
-export([
    get_message/1,
    get_message/2,
    get_message_for_me/1,
    get_message_for_me/2,
    get_message_from_me/1,
    get_message_from_me/2,
    create_message/1,
    update_message/1,
    delete_message_for_me/1,
    delete_message_from_me/1
]).


%%
%% API Functions
%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          УТИЛИТАРНЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Тип разрешения: не рассмотрен, запрещена, разрешена
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_oktype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:create_oktype(Con, Params)
    end).

update_oktype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:update_oktype(Con, Params)
    end).

get_oktype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:get_oktype(Con, [{isdeleted, false}|Params])
    end).

get_oktype(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_doc:get_oktype(Con, [{isdeleted, false}|Params], Fileds)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Тип документа: Блог, коммент к блогу, галерея,
%%      фото, коммент к фото, attach descr.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_doctype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:create_doctype(Con, Params)
    end).

update_doctype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:update_doctype(Con, Params)
    end).

get_doctype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:get_doctype(Con, [{isdeleted, false}|Params])
    end).

get_doctype(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_doc:get_doctype(Con, [{isdeleted, false}|Params], Fileds)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы контента: обычный, эротический
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_contype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:create_contype(Con, Params)
    end).

update_contype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:update_contype(Con, Params)
    end).

get_contype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:get_contype(Con, [{isdeleted, false}|Params])
    end).

get_contype(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_doc:get_contype(Con, [{isdeleted, false}|Params], Fileds)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Тип доступа к контенту контента (блога и галереи):
%%  приватный, дружеский, открытый.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_acctype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:create_acctype(Con, Params)
    end).

update_acctype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:update_acctype(Con, Params)
    end).

get_acctype(Params)->
    dao:with_transaction(fun(Con)->
        dao_doc:get_acctype(Con, [{isdeleted, false}|Params])
    end).

get_acctype(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_doc:get_acctype(Con, [{isdeleted, false}|Params], Fileds)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы чат-комнат. (страна, тюрьма, ад, рай)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_roomtype(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:create_roomtype(Con, Params)
    end).

update_roomtype(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:update_roomtype(Con, Params)
    end).

get_roomtype(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:get_roomtype(Con, [{isdeleted, false}|Params])
    end).

get_roomtype(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_room:get_roomtype(Con, [{isdeleted, false}|Params], Fileds)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Список языков чата.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_chatlang(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:create_chatlang(Con, Params)
    end).

update_chatlang(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:update_chatlang(Con, Params)
    end).

get_chatlang(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:get_chatlang(Con, [{isdeleted, false}|Params])
    end).

get_chatlang(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_room:get_chatlang(Con, [{isdeleted, false}|Params], Fileds)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Список режимов комнаты: дектатура, демократия и пр.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_regimen(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:create_regimen(Con, Params)
    end).

update_regimen(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:update_regimen(Con, Params)
    end).

get_regimen(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:get_regimen(Con, [{isdeleted, false}|Params])
    end).

get_regimen(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_room:get_regimen(Con, [{isdeleted, false}|Params], Fileds)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Дерево тем комнаты
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
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_topic(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:create_topic(Con, Params)
    end).

update_topic(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:update_topic(Con, Params)
    end).

get_topic(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:get_topic(Con, [{isdeleted, false}|Params])
    end).

get_topic(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_room:get_topic(Con, [{isdeleted, false}|Params], Fileds)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы сообществ (обычные, тайные)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_communitytype(Params)->
    dao:with_transaction(fun(Con)->
        dao_community:create_communitytype(Con, Params)
    end).

update_communitytype(Params)->
    dao:with_transaction(fun(Con)->
        dao_community:update_communitytype(Con, Params)
    end).

get_communitytype(Params)->
    dao:with_transaction(fun(Con)->
        dao_community:get_communitytype(Con, [{isdeleted, false}|Params])
    end).

get_communitytype(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_community:get_communitytype(Con, [{isdeleted, false}|Params], Fileds)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы сообщений
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_messagetype(Params)->
    dao:with_transaction(fun(Con)->
        dao_message:create_messagetype(Con, Params)
    end).

update_messagetype(Params)->
    dao:with_transaction(fun(Con)->
        dao_message:update_messagetype(Con, Params)
    end).

get_messagetype(Params)->
    dao:with_transaction(fun(Con)->
        dao_message:get_messagetype(Con, [{isdeleted, false}|Params])
    end).

get_messagetype(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_message:get_messagetype(Con, [{isdeleted, false}|Params], Fileds)
    end).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          ЗНАЧИМЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Блоги
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_blog(Params)->
    dao:with_transaction(fun(Con)->
        dao_blog:create(Con, Params)
    end).

update_blog(Params)->
    dao:with_transaction(fun(Con)->
        dao_blog:update(Con, Params)
    end).

get_blog(Params)->
    dao:with_transaction(fun(Con)->
        dao_blog:get(Con, [{isdeleted, false}|Params])
    end).

get_blog(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_blog:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

delete_blog(Params)->
    dao:with_transaction(fun(Con)->
        dao_blog:update(Con, [{isdeleted, true}|Params])
    end).

is_blog_owner(Uid, Oid)->
    dao:with_transaction(fun(Con)->
        dao_blog:is_owner(Con, Uid, Oid)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Посты 
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_post(Params)->
    dao:with_transaction(fun(Con)->
        dao_post:create(Con, Params)
    end).

update_post(Params)->
    dao:with_transaction(fun(Con)->
        dao_post:update(Con, Params)
    end).

delete_post(Params)->
    dao:with_transaction(fun(Con)->
        dao_post:update(Con, [{isdeleted, true}|Params])
    end).

get_post(Params)->
    dao:with_transaction(fun(Con)->
        dao_post:get(Con, [{isdeleted, false}|Params])
    end).

get_post(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_post:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_post_owner(Uid, Oid)->
    dao:with_transaction(fun(Con)->
        dao_post:is_owner(Con, Uid, Oid)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Kоменты
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%
%% @doc Добавляет комментарий, 
%% Увеливает счетчик комментариев у поста и блога.
%% Все действия в одной транзакции.
%%
create_comment(Params)->
    dao:with_transaction(fun(Con)->
        %% Создаем
        Res = dao_comment:create(Con, Params),
        case proplists:get_value(parent_id, Params) of
            undefined ->
                Res;
            Parent_id ->
                %% Увеличиваем счетчик комментариев у поста,
                %% к которому относитсится этот комментарий
                case dao_post:update(Con, [
                    {filter, [{id, Parent_id}]},
                    {values, [{ncomments, {incr, 1}}]},
                    {fields, [parent_id]}
                ]) of
                    {ok, [{Postpl}]} ->
                        %% Увеличиваем счетчик комментариев у блога,
                        %% к которому относитсится пост этотого комментария.
                        dao_blog:update(Con, [
                            {filter, [
                                {id, proplists:get_value(parent_id, Postpl)}
                            ]},
                            {values, [{ncomments, {incr, 1}}]}
                        ]),     
                        Res;
                    {Eclass, Error} ->
                        {Eclass, Error}            
                end
        end
    end).

update_comment(Params)->
    dao:with_transaction(fun(Con)->
        dao_comment:update(Con, Params)
    end).

%%
%% @doc Удаляет комментарий, 
%% Уменьшает счетчик комментариев у поста и блога. 
%% Все действия в одной транзакции.
%%
delete_comment(Filter)->
    dao:with_transaction(fun(Con)->
        %% Удаляем
        {ok, [{Respl}]}= Res = dao_comment:update(Con, [
            {values, [{isdeleted, true}]}, 
            {filter, [{isdeleted, false}|Filter]},
            {fields, [parent_id]}
        ]),
        io:format("Respl = ~p~n", [Respl]),
        %% Уменьшаем счетчик комментариев у поста,
        %% к которому относитсится этот комментарий.
        case dao_post:update(Con, [
            {filter, [{id, proplists:get_value(parent_id, Respl)}]},
            {values, [{ncomments, {decr, 1}}]},
            {fields, [parent_id]}
        ]) of
            {ok, [{Postpl}]} ->
                %% Уменьшаем счетчик комментариев у блога,
                %% к которому относитсится пост этотого комментария.
                dao_blog:update(Con, [
                    {filter, [{id, proplists:get_value(parent_id, Postpl)}]},
                    {values, [{ncomments, {decr, 1}}]}
                ]),     
                Res;
            {Eclass, Error} ->
                {Eclass, Error}
        end
    end).

get_comment(Params)->
    dao:with_transaction(fun(Con)->
        dao_comment:get(Con, [{isdeleted, false}|Params])
    end).

get_comment(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_comment:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_comment_owner(Uid, Oid)->
    dao:with_transaction(fun(Con)->
        dao_comment:is_owner(Con, Uid, Oid)
    end).
    
    
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Чат-комнаты (комнаты)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_room(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:create(Con, Params)
    end).

update_room(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:update(Con, Params)
    end).

delete_room(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:update(Con, [{isdeleted, true}|Params])
    end).

get_room(Params)->
    dao:with_transaction(fun(Con)->
        dao_room:get(Con, [{isdeleted, false}|Params])
    end).

get_room(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_room:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_room_owner(Uid, Oid)->
    dao:with_transaction(fun(Con)->
        dao_room:is_owner(Con, Uid, Oid)
    end).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Сообщества
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_community(Params)->
    dao:with_transaction(fun(Con)->
        dao_community:create(Con, Params)
    end).

update_community(Params)->
    dao:with_transaction(fun(Con)->
        dao_community:update(Con, Params)
    end).

delete_community(Params)->
    dao:with_transaction(fun(Con)->
        dao_community:update(Con, [{isdeleted, true}|Params])
    end).

get_community(Params)->
    dao:with_transaction(fun(Con)->
        dao_community:get(Con, [{isdeleted, false}|Params])
    end).

get_community(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_community:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_community_owner(Uid, Oid)->
    dao:with_transaction(fun(Con)->
        dao_community:is_owner(Con, Uid, Oid)
    end).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Сообщения
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_message(Params)->
    dao:with_transaction(fun(Con)->
        dao_message:create(Con, Params)
    end).

update_message(Params)->
    dao:with_transaction(fun(Con)->
        dao_message:update(Con, Params)
    end).

delete_message_for_me(Params)->
    dao:with_transaction(fun(Con)->
        dao_message:update(Con, [{isdfr, true},Params])
    end).


delete_message_from_me(Params)->
    dao:with_transaction(fun(Con)->
        dao_message:update(Con, [{isdfr, true},Params])
    end).

get_message(Params)->
    dao:with_transaction(fun(Con)->
        dao_message:get(Con, [{isdeleted, false}|Params])
    end).

get_message(Params, Fileds)->
    dao:with_transaction(fun(Con)->
        dao_message:get(Con, [{isdeleted, false}|Params], Fileds)
    end).

is_message_owner(Uid, Oid)->
    dao:with_transaction(fun(Con)->
        dao_message:is_owner(Con, Uid, Oid)
    end).

get_message_for_me(Params)->
    get_message([{isdfr, false},Params]).

get_message_for_me(Params, Fields)->
    get_message([{isdfr, false},Params], Fields).

get_message_from_me(Params)->
    get_message([{isdfo, false},Params]).

get_message_from_me(Params, Fields)->
    get_message([{isdfo, false},Params], Fields).


%%
%% Local Functions
%%

