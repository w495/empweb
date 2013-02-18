%%
%% @file    empweb_biz_doc.erl
%%          Бизнес логика по работе с документами,
%%          и связанными с ними объектами.
%% 
%%          Зависит от модуля empdb_biz_doc. Все внешние функуции принимают
%%              proplist()
%%          и возвращают:
%%              {ok, [Obj::{proplist()}]}
%%              |   {error, Reason::atom()}
%%              |   {error, {Reason::atom(), Info::atom()}}
%%              |   {error, {Reason::atom(), Info::[Obj::{proplist()}]}}
%%
-module(empweb_biz_doc).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include_lib("empdb/include/empdb.hrl").

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
%% Типы чат-комнат. (страна, тюрьма, ад, рай)
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
%% Список режимов комнаты
%%
-export([
    get_regimen/1,
    get_regimen/2,
    create_regimen/1,
    update_regimen/1
]).

%%
%% Дерево тем комнаты
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
%% Посты \ коменты
%%
-export([
    get_post/1,
    get_post/2,
    create_post/1,
    repost_post/1,
    update_post/1,
    delete_post/1
]).


%%
%% Посты \ коменты
%%
-export([
    get_comment/1,
    get_comment/2,
    repost_comment/1,
    create_comment/1,
    update_comment/1,
    delete_comment/1
]).


%%
%% Сообщества
%%
-export([
    get_community/1,
    get_community/2,
    count_community/1,
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
    readall_message_for_me/1,
    get_message_from_me/1,
    get_message_from_me/2,
    create_message/1,
    update_message/1,
    delete_message_for_me/1,
    delete_message_from_me/1,
    count_message_for_me/1,
    count_message_from_me/1,
    count_message/1,
    count_message_types/1
]).


%% ==========================================================================
%% Внешние функции
%% ==========================================================================


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          УТИЛИТАРНЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Тип разрешения: не рассмотрен, запрещена, разрешена
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_oktype(Params)->
    empdb_biz_doc:create_oktype(Params).

update_oktype(Params)->
    empdb_biz_doc:update_oktype(Params).

get_oktype(Params)->
    get_oktype(Params, []).

get_oktype(Params, Fields)->
    empdb_biz_doc:get_oktype(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Тип документа: Блог, коммент к блогу, галерея,
%%      фото, коммент к фото, attach descr.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_doctype(Params)->
    empdb_biz_doc:create_doctype(Params).

update_doctype(Params)->
    empdb_biz_doc:update_doctype(Params).

get_doctype(Params)->
    get_doctype(Params, []).

get_doctype(Params, Fields)->
    empdb_biz_doc:get_doctype(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы контента: обычный, эротический
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_contype(Params)->
    empdb_biz_doc:create_contype(Params).

update_contype(Params)->
    empdb_biz_doc:update_contype(Params).

get_contype(Params)->
    empdb_biz_doc:get_contype(Params).

get_contype(Params, Fields)->
    empdb_biz_doc:get_contype(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Тип доступа к контенту контента (блога и галереи):
%%  приватный, дружеский, открытый.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_acctype(Params)->
    empdb_biz_doc:create_acctype(Params).

update_acctype(Params)->
    empdb_biz_doc:update_acctype(Params).

get_acctype(Params)->
    empdb_biz_doc:get_acctype(Params).

get_acctype(Params, Fields)->
    empdb_biz_doc:get_acctype(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы чат-комнат. (страна, тюрьма, ад, рай)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_roomtype(Params)->
    empdb_biz_doc:create_roomtype(Params).

update_roomtype(Params)->
    empdb_biz_doc:update_roomtype(Params).

get_roomtype(Params)->
    empdb_biz_doc:get_roomtype(Params).

get_roomtype(Params, Fields)->
    empdb_biz_doc:get_roomtype(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Список языков чата.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_chatlang(Params)->
    empdb_biz_doc:create_chatlang(Params).

update_chatlang(Params)->
    empdb_biz_doc:update_chatlang(Params).

get_chatlang(Params)->
    empdb_biz_doc:get_chatlang(Params).

get_chatlang(Params, Fields)->
    empdb_biz_doc:get_chatlang(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Список режимов комнаты
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_regimen(Params)->
    empdb_biz_doc:create_regimen(Params).

update_regimen(Params)->
    empdb_biz_doc:update_regimen(Params).

get_regimen(Params)->
    empdb_biz_doc:get_regimen(Params).

get_regimen(Params, Fields)->
    empdb_biz_doc:get_regimen(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Дерево тем комнаты
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_topic(Params)->
    empdb_biz_doc:create_topic(Params).

update_topic(Params)->
    empdb_biz_doc:update_topic(Params).

get_topic(Params)->
    empdb_biz_doc:get_topic(Params).

get_topic(Params, Fields)->
    empdb_biz_doc:get_topic(Params, Fields).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы сообществ (обычные, тайные)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_communitytype(Params)->
    empdb_biz_doc:create_communitytype(Params).

update_communitytype(Params)->
    empdb_biz_doc:update_communitytype(Params).

get_communitytype(Params)->
    empdb_biz_doc:get_communitytype(Params).

get_communitytype(Params, Fields)->
    empdb_biz_doc:get_communitytype(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Типы сообщений
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_messagetype(Params)->
    empdb_biz_doc:create_messagetype(Params).

update_messagetype(Params)->
    empdb_biz_doc:update_messagetype(Params).

get_messagetype(Params)->
    empdb_biz_doc:get_messagetype(Params).

get_messagetype(Params, Fields)->
    empdb_biz_doc:get_messagetype(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          ЗНАЧИМЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Блоги
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_blog(Params)->
    empdb_biz_doc:create_blog(Params).

update_blog(Params)->
    empdb_biz_doc:update_blog(Params).


delete_blog(Params)->
    empdb_biz_doc:delete_blog(Params).

    
get_blog(Params)->
    get_blog(Params, []).

get_blog(Params, Fields)->
    empdb_biz_doc:get_blog(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Посты \ коменты
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


create_post(Params)->
    empdb_biz_doc:create_post(Params).

repost_post(Params)->
    empdb_biz_doc:repost_post(Params).

update_post(Params)->
    empdb_biz_doc:update_post(Params).


delete_post(Params)->
    empdb_biz_doc:delete_post(Params).

get_post(Params)->
    empdb_biz_doc:get_post(Params).

get_post(Params, Fields)->
    empdb_biz_doc:get_post(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Посты \ коменты
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


repost_comment(Params)->
    empdb_biz_doc:repost_comment(Params).

create_comment(Params)->
    empdb_biz_doc:create_comment(Params).

update_comment(Params)->
    empdb_biz_doc:update_comment(Params).


delete_comment(Params)->
    empdb_biz_doc:delete_comment(Params).

get_comment(Params)->
    empdb_biz_doc:get_comment(Params).

get_comment(Params, Fields)->
    empdb_biz_doc:get_comment(Params, Fields).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Сообщества
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_community(Params)->
    empdb_biz_doc:create_community(Params).

update_community(Params)->
    empdb_biz_doc:update_community(Params).

delete_community(Params)->
    empdb_biz_doc:update_room(Params).

count_community(Params)->
    empdb_biz_doc:count_community(Params).

get_community(Params)->
    empdb_biz_doc:get_community(Params).

get_community(Params, Fields)->
    empdb_biz_doc:get_community(Params, Fields).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Сообщения
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_message(Params)->
    empdb_biz_doc:create_message(Params).

update_message(Params)->
    empdb_biz_doc:update_message(Params).

get_message(Params)->
    empdb_biz_doc:get_message(Params).

get_message(Params, Fields)->    
    empdb_biz_doc:get_message(Params, Fields).

get_message_for_me(Params)->
    empdb_biz_doc:get_message_for_me(Params).

get_message_from_me(Params)->
    empdb_biz_doc:get_message_from_me(Params).

readall_message_for_me(Params)->
    empdb_biz_doc:readall_message_for_me(Params).

get_message_for_me(Params, Fields)->
    empdb_biz_doc:get_message_for_me(Params, Fields).

get_message_from_me(Params, Fields)->
    empdb_biz_doc:get_message_from_me(Params, Fields).

delete_message(Params)->
    empdb_biz_doc:delete_message(Params).

delete_message_for_me(Params)->
    empdb_biz_doc:delete_message_for_me(Params).

delete_message_from_me(Params)->
    empdb_biz_doc:delete_message_from_me(Params).

count_message_for_me(Params)->
    empdb_biz_doc:count_message_for_me(Params).

count_message_from_me(Params)->
    empdb_biz_doc:count_message_from_me(Params).

count_message(Params) ->
    empdb_biz_doc:count_message(Params).

count_message_types(Params) ->
    empdb_biz_doc:count_message_types(Params).
