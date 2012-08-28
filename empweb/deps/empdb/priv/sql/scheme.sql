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
 *  Нумерация сущностей для которых требуется перевод
 *  Нужно для обеспечения уникальность ti,
 *  для каждой конкретной сущности, которую будем переводить
**/
create sequence seq_any_ti;


/**
 *  Язык
**/
create sequence seq_lang_id;
create table lang(
    id              decimal primary key default nextval('seq_lang_id'),
    alias           varchar(1024) unique,
    name_ti         decimal unique default nextval('seq_any_ti'),
    descr           varchar(1024),
    created         timestamp without time zone not null default now(),
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
    id              decimal primary key default nextval('seq_trtype_id'),
    alias           varchar(1024) unique,
    name_ti         decimal unique default nextval('seq_any_ti'),
    descr           varchar(1024),
    created         timestamp without time zone not null default now(),
    isdeleted       bool default false
);


/**
 *  Многоязыковое содержимое
**/
create sequence seq_tr_id;
create table tr(
    id          decimal primary key default nextval('seq_tr_id'),
    /**
        Имя таблицы с которой связан перевод сущности
    **/
    tt          varchar(1024)   default null,
    /**
        Имя поля с которым связан перевод сущности
    **/
    tf          varchar(1024)   default null,
    /**
        Номер языковой сущности, он не уникален в этой таблице.
        Если не указан, то используется новое значение.
    **/
    ti          decimal default nextval('seq_any_ti'),
    /**
        Краткое описание, оно не уникален в этой таблице.
    **/
    ta          varchar(1024)   default null,
    lang_id     decimal references lang(id)   default null,
    /**
        Типы многоязыкового содержимого можно сделать булевским полем.
        Но возможно, будет много типов.
    **/
    type_id     decimal references trtype(id)   default null,
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
    id          decimal primary key default nextval('seq_filetype_id'),
    name_ti     decimal unique      default nextval('seq_any_ti'),
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
    id         decimal primary key default nextval('seq_fileinfo_id'),
    size       numeric                          default null,
    path       varchar(1024)                    default null,
    name       varchar(1024)                    default null,
    dir        varchar(1024)                    default null,
    type_id    decimal references filetype(id)      default null,
    created    timestamp without time zone not null default now(),
    isdeleted  bool default false
);

/**
 *  Сам по себе файл
**/
create sequence seq_file_id;
create table file(
    id          decimal primary key default nextval('seq_file_id'),
    /**
        Информация о загрузке
    **/
    ulfileinfo    decimal references fileinfo(id)    default null,
    /**
        Информация о скачивании
    **/
    dlfileinfo    decimal references fileinfo(id)    default null,
    /**
        Информация о файле на файловой системе
    **/
    fileinfo      decimal references fileinfo(id)    default null,
    issystem      bool default false,
    created       timestamp without time zone not null default now(),
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
    id          decimal primary key default nextval('seq_emotion_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default now(),
    isdeleted   bool default false
);

/**
 *  Авторитет пользователя
**/
create sequence seq_authority_id;
create table authority(
    id          decimal primary key default nextval('seq_authority_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    level       decimal default 0,
    created     timestamp without time zone not null default now(),
    isdeleted   bool default false
);

/**
 *  Статус пользователя: оnline/offline/забанен
**/
create sequence seq_pstatus_id;
create table pstatus(
    id          decimal primary key default nextval('seq_pstatus_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default now(),
    isdeleted   bool default false
);


/**
 *  Семейный статус пользователя
**/
create sequence seq_mstatus_id;
create table mstatus(
    id          decimal primary key default nextval('seq_mstatus_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique default nextval('seq_any_ti'),
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
--     id          decimal primary key default nextval('seq_gender_id'),
--     /**
--         Номер языковой сущности
--     **/
--     name_ti     decimal unique default nextval('seq_any_ti'),
--     alias       varchar(1024) unique,
--     isdeleted     bool default false
-- );
-- -- ----------------------------------------------------------------------
-- --


create sequence seq_perspichead_id;
create table perspichead(
    id          decimal primary key default nextval('seq_perspichead_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique default nextval('seq_any_ti'),
    file_id     decimal references file(id) default null,
    created     timestamp without time zone not null default now(),
    isdeleted   bool default false
);

create sequence seq_perspicbody_id;
create table perspicbody(
    id          decimal primary key default nextval('seq_perspicbody_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique default nextval('seq_any_ti'),
    file_id     decimal references file(id) default null,
    created     timestamp without time zone not null default now(),
    isdeleted   bool default false
);

/**
 *  Города реального мира
**/

create sequence seq_pregion_id;
create table pregion(
    id          decimal primary key default nextval('seq_perspicbody_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique default nextval('seq_any_ti'),
    alias       varchar(1024),
    pregion_id  decimal references pregion(id)     default null,
    created     timestamp without time zone not null default now(),
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
    id decimal primary key default nextval('seq_pers_id'),
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
    pregion_id  decimal references  pregion(id)     default null,
    birthday    timestamp       without time zone not null default now(),
    -- gender_id           decimal references gender(id)      default null,
    lang_id     decimal     references lang(id) default null,
    ismale      bool    default false,
    /**
        ------------------------------------------------------------
            Информация о персоонаже
        ------------------------------------------------------------
    **/
    money               real,
    pstatus_id          decimal references pstatus(id)     default null,
    authority_id        decimal references authority(id)   default null,
    emotion_id          decimal references emotion(id)     default null,

    mstatus_id          decimal references mstatus(id)     default null,
    married_id          decimal references pers(id)        default null,
    mother_id           decimal references pers(id)        default null,
    father_id           decimal references pers(id)        default null,
    /** Общество в котором он состоит
        [см далее]: community_id decimal references community(id) default null,
    **/
    /** Страна \ рай \ aд
        [см далее]: room_id decimal references room(id) default null,
    **/
    allowauctionoffer   bool default false,
    perspicbody_id      decimal references perspicbody(id)   default null,
    perspichead_id      decimal references perspichead(id)   default null,
    /**
        ------------------------------------------------------------
            Внутрениие поля
        ------------------------------------------------------------
    **/
    created             timestamp without time zone not null default now(),
    isdeleted           bool default false
);

alter table file add column owner_id
    decimal references pers(id) default null;

/**
 *  Группа пользователей
**/
create sequence seq_group_id;
create table pgroup (
    id decimal primary key default nextval('seq_group_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique default nextval('seq_any_ti'),
    alias           varchar(1024)   unique,
    issystem        bool    default false,
    isdeleted       bool    default false
);

/**
 *  Типы прав
**/
create sequence seq_permtype_id;
create table permtype (
    id          decimal primary key default nextval('seq_permtype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique default nextval('seq_any_ti'),
    alias       varchar(1024)   unique
);

/**
 *  Типы cущностей прав
**/
create sequence seq_permentitytype_id;
create table permentitytype (
    id          decimal primary key default nextval('seq_permentitytype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique default nextval('seq_any_ti'),
    alias       varchar(1024)   unique
);


/**
 *  Права
**/
create sequence seq_perm_id;
create table perm (
    id              decimal primary key default nextval('seq_perm_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique default nextval('seq_any_ti'),
    alias           varchar(1024) unique,
    permtype_id     decimal references permtype(id),
    entitytype_id   decimal references permentitytype(id),
    entity_id       int,
    type            int
);

/**
 *  Друзья
**/
create sequence seq_friend_id;
create table friend(
    id          decimal primary key default     nextval('seq_friend_id'),
    /**
        Номер языковой сущности
    **/
    /*name_ti     decimal unique default nextval('seq_any_ti'),*/
    pers_id     decimal references pers(id)    default null,
    friend_id   decimal references pers(id)    default null,
    constraint  friend_pers_id_friend_id_many_key unique (pers_id, friend_id)
);


/****************************************************************************
    =====================================================================
                                НАСТРОЙКИ
    =====================================================================
****************************************************************************/

/**
 * Типы системных переменных
**/
create sequence seq_sysvartype_id;
create table sysvartype(
    id              decimal primary key default nextval('seq_sysvartype_id'),
    alias           varchar(1024) not null unique,
    name_ti         decimal unique default nextval('seq_any_ti'),
    created         timestamp without time zone not null default now(),
    isdeleted       bool default false
);

/**
 * Cистемные переменные
**/
create sequence seq_sysvar_id;
create table sysvar(
    id              decimal primary key default nextval('seq_sysvar_id'),
    perm_id         decimal references perm(id) default null,
    type_id         decimal references sysvartype(id) default null,
    alias           varchar(1024) not null unique,
    val             varchar(1024) not null,
    sysvar_id       decimal references sysvar(id),
    name_ti         decimal unique default nextval('seq_any_ti'),
    created         timestamp without time zone not null default now(),
    isdeleted       bool default false
);


/****************************************************************************
    =====================================================================
                                ДОКУМЕНТЫ
    =====================================================================
****************************************************************************/



/**
 *  Тип разрешения: не рассмотрен, запрещена, разрешена
**/
create sequence seq_oktype_id;
create table oktype(
    id          decimal primary key default nextval('seq_oktype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    isdeleted   bool default false
);



/**
 *  Тип доступа к контенту контента (блога и галереиc):
 *      Приватный, дружеский, открытый.
**/
create sequence seq_acctype_id;
create table acctype(
    id              decimal primary key default     nextval('seq_acctype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique default nextval('seq_any_ti'),
    alias           varchar(1024)   unique,
    isdeleted       bool default false
);

/**
 *  Типы контента:
 *      Обычный, эротический
**/
create sequence seq_contype_id;
create table contype(
    id              decimal primary key default     nextval('seq_contype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique default nextval('seq_any_ti'),
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
    id              decimal primary key default     nextval('seq_doctype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique default nextval('seq_any_ti'),
    alias           varchar(1024)   unique,
    isdeleted       bool default false
);


/**
 *  Документ
**/
create sequence seq_doc_id;
create table doc(
    id                  decimal primary key default     nextval('seq_doc_id'),
    head                text,
    body                text default null,

    /**
        Непросмотрен, разрешен, запрещен, там где это нужно,
    **/
    oktype_id    decimal references oktype(id) default null,

    /**
        Тип документа: блог, коммент к блогу, галерея,
            фото, коммент к фото, attach descr.
    **/
    doctype_id          decimal references doctype(id)      default null,

    /**
        Типы контента: Обычный, эротический
    **/
    contype_id          decimal references contype(id)      default null,
    --     /**
    --         Разрешение на чтение
    --     **/
    --     read_acctype_id     decimal references acctype(id)      default null,
    --     /**
    --         Разрешение комментов
    --     **/
    --     comm_acctype_id     decimal references acctype(id)      default null,
    --
    owner_id            decimal references pers(id)         default null,
    parent_id           decimal references doc(id)          default null,


    /**
        количество просмотров документа
    **/
    view_counter        decimal default null,

    /**
        позиция в списке
    **/
    position            numeric default null,
    created             timestamp without time zone not null default now(),
    isdeleted           bool default false
);


------------------------------------------------------------------------------
-- Аттачи
------------------------------------------------------------------------------

create sequence seq_atttype_id;
create table atttype(
    id              decimal primary key default     nextval('seq_atttype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique default nextval('seq_any_ti'),
    alias           varchar(1024)   unique,
    isdeleted         bool default false
);

create table att(
    doc_id      decimal unique references doc(id)       default null,
    type_id     decimal references atttype(id)          default null,
    file_id     decimal references file(id)    default null
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
    doc_id              decimal unique references doc(id),
    /**
        Разрешение на чтение
    **/
    read_acctype_id     decimal references acctype(id)     default null,
    /**
        Разрешение комментов
    **/
    comm_acctype_id     decimal references acctype(id)     default null
);

/**
 *  Запись блога \ комментарий
**/
create table post(
    doc_id              decimal unique references doc(id),
    /**
        Разрешение на чтение
    **/
    read_acctype_id     decimal references acctype(id)     default null,
    /**
        Разрешение комментов
    **/
    comm_acctype_id     decimal references acctype(id)     default null,
    /**
        Оповещение комментов
    **/
    commnotice          bool default false
);

-- /**
--  *  Опрос
-- **/
-- create table pool(
--     doc_id              decimal unique references doc(id),
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
    doc_id              decimal unique references doc(id),
    /**
        Разрешение на чтение
    **/
    read_acctype_id     decimal references acctype(id)     default null,
    /**
        Разрешение комментов
    **/
    comm_acctype_id     decimal references acctype(id)     default null,
    /**
        Разрешение на перепост
    **/
    repost              bool default false
);

/**
 *  Картинка галереи
**/
create table gpic(
    att_id              decimal unique references att(doc_id)
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
    id          decimal primary key default nextval('seq_roomtype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    isdeleted     bool default false
);

/**
 *  Список языков чата. Не обязан пересекаться с таблицей lang.
**/
create sequence seq_chatlang_id;
create table chatlang(
    id          decimal primary key default nextval('seq_chatlang_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    isdeleted     bool default false
);


/**
 *  Дерево тем чата. Редактируется администраторами и пользователями. 
**/
create sequence seq_topic_id;
create table topic(
    id          decimal primary key default nextval('seq_topic_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    /**
        Номер языковой сущности
    **/
    descr_ti    decimal unique      default nextval('seq_any_ti'),
    -- alias       varchar(1024)   unique,
    topic_id    decimal references topic(id) default null,
    isdeleted   bool default false
);


create table room(
    doc_id              decimal unique references doc(id),
    type_id             decimal references roomtype(id) default null,
    ulimit              decimal default null,
    chatlang_id         decimal references chatlang(id) default null,
    topic_id            decimal references topic(id) default null,
    slogan              text default null,
    weather             text default null,
    treasury            decimal default null
--     bearing - герб
--     flag - ссылка на картинку флага
--     wallpaper - ссылка на картинку фона ?????? не закончено
);


alter table pers add  column room_id
    decimal references room(doc_id) default null;

------------------------------------------------------------------------------
-- Сообщество
------------------------------------------------------------------------------

/**
 *  Типы сообществ (обычные, тайные)
**/
create sequence seq_communitytype_id;
create table communitytype(
    id          decimal primary key default nextval('seq_communitytype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    isdeleted   bool default false
);



create table community(
    doc_id              decimal unique references doc(id),
    type_id             decimal references communitytype(id) default null,
    slogan              text default null,
    treasury            decimal default null
);

alter table pers add column community_id
    decimal references community(doc_id) default null;

-------------------------------------------------------------------------------
-- СВЯЗКИ МНОГИЕ КО МНОГИМ
-------------------------------------------------------------------------------

/**
 *  Многие ко многим для прав и групп
**/
create table perm2pgroup (
    perm_id decimal references perm(id) not null,
    group_id decimal references pgroup(id) not null
);

/**
 *  Многие ко многим для пользователей и групп
**/
create table pers2pgroup (
    pers_id decimal references pers(id) not null,
    group_id decimal references pgroup(id) not null
);

