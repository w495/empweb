-module(empweb_norm_pers).

-include("empweb.hrl").

-include_lib("norm/include/norm.hrl").

%%
%% Описание записей событий и макросов
%%
-include_lib("evman/include/events.hrl").


%%
%% Трансформация для получения имени функции.
%%
-include_lib("evman/include/evman_transform.hrl").


-export([
    norm/1
]).

norm('count') ->
    norm('get');

%%
%% @doc Общие функции нормировки для всех документов
%%
norm('get') ->
    [
        #norm_rule{
            key         = id,
            required    = false,
            types       = empweb_norm:filter([integer])
        },
        #norm_rule{
            key         = nick,
            required    = false,
            types       = empweb_norm:filter([string])
        },
        #norm_rule{
            key         = perspichead_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = perspicbody_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = perspicphoto_id,
            required    = false,
            types       = [nullable, integer]
        },
        #norm_rule{
            key         = image_width,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
            %default     = null
        },
        #norm_rule{
            key         = image_height,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
            %default     = null
        },
        #norm_rule{
            key         = costume_thingbuy_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
    %% ----------------------------------------------------
    %% Имя
        #norm_rule{
            key         = fname,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
    %% Фамилия
        #norm_rule{
            key         = sname,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
    %% Работает или нет
        #norm_rule{
            key         = isempl,
            required    = false,
            types       = empweb_norm:filter([nullable, boolean])
        },
    %% Род занятий
        #norm_rule{
            key         = empl,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
    %% Xобби
        #norm_rule{
            key         = hobby,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
    %% Интерес
        #norm_rule{
            key         = interest,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
    %% Oписание
        #norm_rule{
            key         = descr,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
    %% Регион pregion_id
        #norm_rule{
            key         = pregion_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
    %% Дата рождения
        #norm_rule{
            key         = birthday,
            required    = false,
            types       = empweb_norm:filter([nullable, unixdatetime])
        },
    %% Флаг пола
        #norm_rule{
            key         = ismale,
            required    = false,
            types       = empweb_norm:filter([nullable, boolean])
        },
    %% Комната, где находится
        #norm_rule{
            key         = live_room_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
    %% Комната, где гражданин
        #norm_rule{
            key         = citizen_room_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
    %% Положение в комнате
        #norm_rule{
            key         = live_room_pos,
                required   = false,
            types       = empweb_norm:filter([nullable, 'float'])
        },
    %% Тип комнаты
        #norm_rule{
            key         = live_roomtype_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = live_roomtype_alias,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
        #norm_rule{
            key         = isprisoner,
            required    = false,
            types       = empweb_norm:filter([nullable, boolean])
        },
    %% Сообщество
        #norm_rule{
            key         = live_community_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = live_community_approved,
            required    = false,
            types       = empweb_norm:filter([nullable, boolean])
        },
        #norm_rule{
            key         = live_community_rejectreason,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
    %% Эмоции пользователя.
        #norm_rule{
            key         = emotion_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = emotion_alias,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
    %% Чиновничий статус пользователя
        #norm_rule{
            key         = ostatus_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = ostatus_alias,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
        #norm_rule{
            key         = isostatusable,
            required    = false,
            types       = empweb_norm:filter([nullable, boolean])
        },
    %% Статус пользователя
        #norm_rule{
            key         = pstatus_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = pstatus_alias,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
    %% Язык пользователя.
        #norm_rule{
            key         = lang_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key         = lang_alias,
            required    = false,
            types       = empweb_norm:filter([nullable, string])
        },
    %% Уровень показа денег
        #norm_rule{
            key = show_money_acctype_id,
            required    = false,
            types = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key = show_money_acctype_alias,
            required    = false,
            types = empweb_norm:filter([nullable, string])
        },
    %% Уровень получения сообщений.
        #norm_rule{
            key = get_message_acctype_id,
            required    = false,
            types = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key = get_message_acctype_alias,
            required    = false,
            types = empweb_norm:filter([nullable, string])
        },
    %% Уровень принятия подарков.
        #norm_rule{
            key = get_thingbuy_acctype_id,
            required    = false,
            types = empweb_norm:filter([nullable, integer])
        },
        #norm_rule{
            key = get_thingbuy_acctype_alias,
            required    = false,
            types = empweb_norm:filter([nullable, string])
        },
    %% Реальное место положение
        #norm_rule{
            key         = geo_id,
            required    = false,
            types       = empweb_norm:filter([nullable, integer])
        }
        |empweb_norm:norm('get')
    ];

norm(_) ->
    %%
    %% empweb_norm:norm(_)
    %%s
    [].

