/***********************************************************************
 *
 *  \file Описание схемы базы данных
 *
***********************************************************************/


/****************************************************************************
    =====================================================================
                                ЯЗЫКИ
    =====================================================================
****************************************************************************/

/**
 *  Язык
**/
create sequence seq_lang_id;
create table lang(
    id              int primary key default nextval('seq_lang_id'),
    alias           varchar(1024) unique,
    descr           varchar(1024),
    created         timestamp without time zone NOT NULL DEFAULT now(),
    isdeleted       bool default false
);

/**
 *  Типы многоязыкового содержимого.
 *      Динамическое, статическое.
 *  Это сделано, для того, чтобы отличать языковые сущности,
 *  для различных записях в различных таблицах и надписи в интерфейсе
**/
create sequence seq_trtype_id;
create table trtype(
    id              int primary key default nextval('seq_trtype_id'),
    alias           varchar(1024) unique,
    descr           varchar(1024),
    created         timestamp without time zone not null default now(),
    isdeleted       bool default false
);

/**
 *  Нумерация сущностей для которых требуется перевод
 *  Нужно для обеспечения уникальность ti,
 *  для каждой конкретной сущности, которую будем переводить
**/
create sequence seq_any_ti;


/**
 *  Многоязыковое содержимое
**/
create sequence seq_tr_id;
create table tr(
    id          int primary key default nextval('seq_tr_id'),
    /**
        Имя таблицы с которой связан перевод сущности
    **/
    tt          varchar(1024)   default null,
    /**
        Номер языковой сущности, он не уникален в этой таблице.
        Если не указан, то используется новое значение.
    **/
    ti          int default nextval('seq_any_ti'),
    /**
        Краткое описание, оно не уникален в этой таблице.
    **/
    ta          varchar(1024)   default null,
    lang_id     int references lang(id)   default null,
    /**
        Типы многоязыкового содержимого можно сделать булевским полем.
        Но возможно, будет много типов.
    **/
    type_id     int references trtype(id)   default null,
    text        text default null,
    isdeleted   bool default false,
    constraint  tr_ti_lang_id_many_key unique (ti,lang_id)
);

/****************************************************************************
    =====================================================================
                                ФАЙЛЫ
    =====================================================================
****************************************************************************/

/**
 *  Тип файла
**/
create sequence seq_filetype_id;
create table filetype(
    id          int primary key default nextval('seq_filetype_id'),
    name_ti     int unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    mime        varchar(1024)   default null,
    ext         varchar(1024)   default null,
    dir         varchar(1024)   default null,
    created     timestamp without time zone not null default now(),
    isdeleted   bool default false
);

/**
 *  Информация о файле
**/
create sequence seq_fileinfo_id;
create table fileinfo(
    id         int primary key default nextval('seq_fileinfo_id'),
    size       numeric                          default null,
    path       varchar(1024)                    default null,
    name       varchar(1024)                    default null,
    dir        varchar(1024)                    default null,
    type_id    int references filetype(id)      default null,
    created    timestamp without time zone NOT NULL DEFAULT now(),
    isdeleted  bool default false
);

/**
 *  Сам по себе файл
**/
create sequence seq_file_id;
create table file(
    id          int primary key default nextval('seq_file_id'),
    /**
        Информация о загрузке
    **/
    ulfileinfo    int references fileinfo(id)    default null,
    /**
        Информация о скачивании
    **/
    dlfileinfo    int references fileinfo(id)    default null,
    /**
        Информация о файле на файловой системе
    **/
    fileinfo      int references fileinfo(id)    default null,
    issystem      bool default false,
    created       timestamp without time zone NOT NULL DEFAULT now(),
    isdeleted     bool default false
);


/****************************************************************************
    =====================================================================
                                ПОЛЬЗОВАТЕЛЬ
    =====================================================================
****************************************************************************/


/**
 *  Эмоция пользователя
**/
create sequence seq_emotion_id;
create table emotion(
    id          int primary key default nextval('seq_emotion_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone NOT NULL DEFAULT now(),
    isdeleted   bool default false
);

/**
 *  Авторитет пользователя
**/
create sequence seq_authority_id;
create table authority(
    id          int primary key default nextval('seq_authority_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    level       int default 0,
    created     timestamp without time zone NOT NULL DEFAULT now(),
    isdeleted   bool default false
);

/**
 *  Статус пользователя: оnline/offline/забанен
**/
create sequence seq_pstatus_id;
create table pstatus(
    id          int primary key default nextval('seq_pstatus_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone NOT NULL DEFAULT now(),
    isdeleted   bool default false
);


/**
 *  Семейный статус пользователя
**/
create sequence seq_mstatus_id;
create table mstatus(
    id          int primary key default nextval('seq_mstatus_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default now(),
    isdeleted   bool default false
);

-- --
-- -- [?]
-- -- Т.к Нам придется переводить статику, то возможно,
-- -- для какиех-то вещей придется сделать отдельные сущности
-- -- с отдельными именованиями.
-- -- ----------------------------------------------------------------------
-- /**
--  *  Пол пользователя
-- **/
-- create sequence seq_gender_id;
-- create table gender(
--     id          int primary key default nextval('seq_gender_id'),
--     /**
--         Номер языковой сущности
--     **/
--     name_ti     int unique default nextval('seq_any_ti'),
--     alias       varchar(1024) unique,
--     isdeleted     bool default false
-- );
-- -- ----------------------------------------------------------------------
-- --


create sequence seq_perspichead_id;
create table perspichead(
    id          int primary key default nextval('seq_perspichead_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique default nextval('seq_any_ti'),
    file_id     int references file(id) default null,
    created     timestamp without time zone NOT NULL DEFAULT now(),
    isdeleted   bool default false
);

create sequence seq_perspicbody_id;
create table perspicbody(
    id          int primary key default nextval('seq_perspicbody_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique default nextval('seq_any_ti'),
    file_id     int references file(id) default null,
    created     timestamp without time zone NOT NULL DEFAULT now(),
    isdeleted   bool default false
);

/**
 *  Города реального мира
**/

create sequence seq_pregion_id;
create table pregion(
    id          int primary key default nextval('seq_perspicbody_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique default nextval('seq_any_ti'),
    alias       varchar(1024),
    pregion_id  int references pregion(id)     default null,
    created     timestamp without time zone NOT NULL DEFAULT now(),
    isdeleted   bool default false,
    constraint  pregion_alias_pregion_id_many_key unique (alias,pregion_id)
);


/**
 *  Пользователь
**/
create sequence seq_pers_fakelogin;
create sequence seq_pers_id;
create table pers(
    /**
        ------------------------------------------------------------
            Идентификация
        ------------------------------------------------------------
    **/
    id int primary key default nextval('seq_pers_id'),
    login               varchar(1024) default '_'
                        || CAST (nextval('seq_pers_fakelogin')
                            as varchar(1024))
                        || '_'
                        || extract(epoch from now())
                        || '_' not null unique,
    nick        varchar(1024),
    phash       char(32)        not null,
    email       varchar(1024)   default null,
    phone       numeric         default null,
    /**
        ------------------------------------------------------------
            Информация о пользователе
        ------------------------------------------------------------
    **/
    fname       varchar(1024)   default null,
    sname       varchar(1024)   default null,
    empl        varchar(1024)   default null,
    hobby       varchar(1024)   default null,
    descr       varchar(1024)   default null,
    pregion_id  int references  pregion(id)     default null,
    birthday    timestamp       without time zone NOT NULL DEFAULT now(),
    -- gender_id           int references gender(id)      default null,
    lang_id     int     references lang(id) default null,
    ismale      bool    default false,
    /**
        ------------------------------------------------------------
            Информация о персоонаже
        ------------------------------------------------------------
    **/
    money               real,
    pstatus_id          int references pstatus(id)     default null,
    authority_id        int references authority(id)   default null,
    emotion_id          int references emotion(id)     default null,

    mstatus_id          int references mstatus(id)     default null,
    married_id          int references pers(id)        default null,
    mother_id           int references pers(id)        default null,
    father_id           int references pers(id)        default null,
    /** Общество в котором он состоит
        [см далее]: community_id int references community(id) default null,
    **/
    /** Страна \ рай \ aд
        [см далее]: room_id int references room(id) default null,
    **/
    allowauctionoffer   bool default false,
    perspicbody_id      int references perspicbody(id)   default null,
    perspichead_id      int references perspichead(id)   default null,
    /**
        ------------------------------------------------------------
            Внутрениие поля
        ------------------------------------------------------------
    **/
    created             timestamp without time zone NOT NULL DEFAULT now(),
    isdeleted           bool default false
);

alter table file add column owner_id
    int references pers(id) default null;

/**
 *  Группа пользователей
**/
create sequence seq_group_id;
create table pgroup (
    id int primary key default nextval('seq_group_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         int unique default nextval('seq_any_ti'),
    alias           varchar(1024)   unique,
    issystem        bool    default false,
    isdeleted       bool    default false
);

/**
 *  Типы прав
**/
create sequence seq_permtype_id;
create table permtype (
    id          int primary key default nextval('seq_permtype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique default nextval('seq_any_ti'),
    alias       varchar(1024)   unique
);

/**
 *  Типы cущностей прав
**/
create sequence seq_permentitytype_id;
create table permentitytype (
    id          int primary key default nextval('seq_permentitytype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique default nextval('seq_any_ti'),
    alias       varchar(1024)   unique
);


/**
 *  Права
**/
create sequence seq_perm_id;
create table perm (
    id              int primary key default nextval('seq_perm_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         int unique default nextval('seq_any_ti'),
    alias           varchar(1024) unique,
    permtype_id     int references permtype(id),
    entitytype_id   int references permentitytype(id),
    entity_id       int,
    type            int
);

/**
 *  Друзья
**/
create sequence seq_friend_id;
create table friend(
    id          int primary key default     nextval('seq_friend_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique default nextval('seq_any_ti'),
    pers_id     int references pers(id)    default null,
    friend_id   int references pers(id)    default null,
    constraint  friend_pers_id_friend_id_many_key unique (pers_id, friend_id)
);


/****************************************************************************
    =====================================================================
                                ДОКУМЕНТЫ
    =====================================================================
****************************************************************************/

/**
 *  Тип доступа к контенту контента (блога и галереи):
 *      Приватный, дружеский, открытый.
**/
create sequence seq_acctype_id;
create table acctype(
    id              int primary key default     nextval('seq_acctype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         int unique default nextval('seq_any_ti'),
    alias           varchar(1024)   unique,
    isdeleted       bool default false
);

/**
 *  Типы контента:
 *      Обычный, эротический
**/
create sequence seq_contype_id;
create table contype(
    id              int primary key default     nextval('seq_contype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         int unique default nextval('seq_any_ti'),
    alias           varchar(1024)   unique,
    isdeleted       bool default false
);

/**
 *  Тип документа:
 *      Блог, коммент к блогу, галерея, фото, коммент к фото,
 *      attach descr.
**/
create sequence seq_doctype_id;
create table doctype(
    id              int primary key default     nextval('seq_doctype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         int unique default nextval('seq_any_ti'),
    alias           varchar(1024)   unique,
    isdeleted       bool default false
);


/**
 *  Документ
**/
create sequence seq_doc_id;
create table doc(
    id                  int primary key default     nextval('seq_doc_id'),
    head                text,
    text                text default null,
    --
    doctype_id          int references doctype(id)      default null,
    contype_id          int references contype(id)      default null,
    --     /**
    --         Разрешение на чтение
    --     **/
    --     read_acctype_id     int references acctype(id)      default null,
    --     /**
    --         Разрешение комментов
    --     **/
    --     comm_acctype_id     int references acctype(id)      default null,
    --
    owner_id            int references pers(id)         default null,
    parent_id           int references doc(id)          default null,
    view_counter        numeric default null,
    position            numeric default null,
    created             timestamp without time zone NOT NULL DEFAULT now(),
    isdeleted           bool default false
);


------------------------------------------------------------------------------
-- Аттачи
------------------------------------------------------------------------------

create sequence seq_atttype_id;
create table atttype(
    id              int primary key default     nextval('seq_atttype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         int unique default nextval('seq_any_ti'),
    alias           varchar(1024)   unique,
    isdeleted         bool default false
);

create table att(
    doc_id      int unique references doc(id)       default null,
    type_id     int references atttype(id)          default null,
    file_id     int references file(id)    default null
);

------------------------------------------------------------------------------
-- Блог
------------------------------------------------------------------------------

/**
 *  Блог
 *  Блог пользователя, общая инфа, настройки
 *  # нужен системный блог - список постов на оценку другими
 *  Избранное - cистемный блог,
 *  создается автоматически каждому пользователю,
 *  куда он может добавлять ссылки на чужие записи.
 *  Используется таблица repost.
**/
create table blog(
    doc_id              int unique references doc(id),
    /**
        Разрешение на чтение
    **/
    read_acctype_id     int references acctype(id)     default null,
    /**
        Разрешение комментов
    **/
    comm_acctype_id     int references acctype(id)     default null
);

/**
 *  Запись блога \ комментарий
**/
create table post(
    doc_id              int unique references doc(id),
    /**
        Разрешение на чтение
    **/
    read_acctype_id     int references acctype(id)     default null,
    /**
        Разрешение комментов
    **/
    comm_acctype_id     int references acctype(id)     default null,
    /**
        Оповещение комментов
    **/
    commnotice          bool default false
);

-- /**
--  *  Опрос
-- **/
-- create table pool(
--     doc_id              int unique references doc(id),
--     /**
--         Разрешение на чтение
--     **/
--     dt_start
--     dt_stop
-- );


------------------------------------------------------------------------------
-- Галерея
------------------------------------------------------------------------------

/**
 *  Галерея
**/
create table gallery(
    doc_id              int unique references doc(id),
    /**
        Разрешение на чтение
    **/
    read_acctype_id     int references acctype(id)     default null,
    /**
        Разрешение комментов
    **/
    comm_acctype_id     int references acctype(id)     default null,
    /**
        Разрешение на перепост
    **/
    repost              bool default false
);

/**
 *  Картинка галереи
**/
create table gpic(
    att_id              int unique references att(doc_id)
);

-- ...

------------------------------------------------------------------------------
-- Чат комнаты \ страны
------------------------------------------------------------------------------


/**
 *  Типы чат-комнат. (страна, тюрьма, ад, рай)
**/
create sequence seq_roomtype_id;
create table roomtype(
    id          int primary key default nextval('seq_roomtype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    isdeleted     bool default false
);

/**
 *  Список языков чата. Не обязан пересекаться с таблицей lang.
**/
create sequence seq_chatlang_id;
create table chatlang(
    id          int primary key default nextval('seq_chatlang_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique      default nextval('seq_any_ti'),
    -- alias       varchar(1024)   unique,
    isdeleted     bool default false
);


/**
 *  Дерево тем чата. Редактируется администраторами и пользователями. 
**/
create sequence seq_topic_id;
create table topic(
    id          int primary key default nextval('seq_topic_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique      default nextval('seq_any_ti'),
    /**
        Номер языковой сущности
    **/
    descr_ti    int unique      default nextval('seq_any_ti'),
    -- alias       varchar(1024)   unique,
    topic_id    int references topic(id) default null,
    isdeleted   bool default false
);


create table room(
    doc_id              int unique references doc(id),
    type_id             int references roomtype(id) default null,
    user_limit          int default null,
    chatlang_id         int references chatlang(id) default null,
    topic_id            int references topic(id) default null,
    slogan              text default null,
    weather             text default null,
    treasury            int default null
--     bearing - герб
--     flag - ссылка на картинку флага
--     wallpaper - ссылка на картинку фона ?????? не закончено
);


alter table pers add  column room_id
    int references room(doc_id) default null;

------------------------------------------------------------------------------
-- Сообщество
------------------------------------------------------------------------------


create sequence seq_communitytype_id;
create table communitytype(
    id          int primary key default nextval('seq_communitytype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     int unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    isdeleted   bool default false
);


create table community(
    doc_id              int unique references doc(id),
    type_id             int references communitytype(id) default null,
    /**
        approve_status
        (nullable bool:
            null - не рассмотрен,
            false - запрещена,
            true - разрешена)
    **/
    approvestatus       bool default null,
    slogan              text default null,
    treasury            int default null

);

alter table pers add column community_id
    int references community(doc_id) default null;

-------------------------------------------------------------------------------
-- СВЯЗКИ МНОГИЕ КО МНОГИМ
-------------------------------------------------------------------------------

/**
 *  Многие ко многим для прав и групп
**/
create table perm2pgroup (
    perm_id int references perm(id) not null,
    group_id int references pgroup(id) not null
);

/**
 *  Многие ко многим для пользователей и групп
**/
create table pers2pgroup (
    pers_id int references pers(id) not null,
    group_id int references pgroup(id) not null
);

