/***********************************************************************
 *
 *  \file Описание схемы базы данных
 *
***********************************************************************/

-- set timezone to 'UTC';

create or replace function utcnow() returns timestamp as $$
        select now() at time zone 'UTC'
$$ language sql;


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
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
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
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
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
    /**
        Типы многоязыкового содержимого можно сделать булевским полем.
        Но возможно, будет много типов.
    **/
    lang_id     decimal         references lang(id)      default null,
    lang_alias  varchar(1024)   references lang(alias)   default null,
    /**
        Типы многоязыкового содержимого можно сделать булевским полем.
        Но возможно, будет много типов.
    **/
    trtype_id       decimal         references trtype(id)       default null,
    trtype_alias    varchar(1024)   references trtype(alias)    default null,
    text            text default null,
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false,
    constraint      tr_ti_lang_id_many_key unique (ti,lang_id)
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
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
);



/**
 *  Тип информации о файле
**/
create sequence seq_fileinfotype_id;
create table fileinfotype(
    id          decimal primary key default nextval('seq_fileinfotype_id'),
    name_ti     decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
);


/**
 *  Информация о файле
**/
create sequence seq_fileinfo_id;
create table fileinfo(
    id              decimal primary key default nextval('seq_fileinfo_id'),
    size            numeric                             default null,
    path            varchar(1024)                       default null,
    name            varchar(1024)                       default null,
    dir             varchar(1024)                       default null,

    tokenlong       decimal                             default null,
    tokenstring     char(128)                           default null,
    md5long         decimal                             default null,
    md5string       char(32)                            default null,

    fileinfotype_id     decimal references fileinfotype(id)     default null,
    fileinfotype_alias  varchar(1024)                   default null,
    filetype_id         decimal references filetype(id)         default null,
    filetype_alias      varchar(1024)                   default null,

    created             timestamp without time zone not null    default utcnow(),
    isdeleted           boolean default false
);

/**
 *  Сам по себе файл
**/
create sequence seq_file_id;
create table file(
    id              decimal primary key default nextval('seq_file_id'),
    /**
        Информация о загрузке
    **/
    ulfileinfo_id   decimal references fileinfo(id)    default null,
    /**
        Информация о скачивании
    **/
    dlfileinfo_id   decimal references fileinfo(id)    default null,
    /**
        Информация о файле на файловой системе
    **/
    fsfileinfo_id   decimal references fileinfo(id)    default null,
    issystem        boolean default false,

    tokenlong       decimal                             default null,
    tokenstring     char(128)                           default null,

    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
);


alter table fileinfo add column
    file_id decimal references file(id) default null;


/****************************************************************************
    =====================================================================
                                ПОЛЬЗОВАТЕЛЬ
    =====================================================================
****************************************************************************/

/**
 *  Типы действий пользователя
**/
create sequence seq_actiontype_id;
create table actiontype(
    id          decimal primary key default nextval('seq_actiontype_id'),
    name_ti     decimal unique  default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    ispaid      boolean         default false,
    price       numeric(1000, 2)          default 0,
    created     timestamp without time zone not null
        default utcnow(),
    isdeleted   boolean default false
);

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
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
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
    
--     next_id     decimal         references authority(id) default null,
--     next_alias  varchar(1024)   references authority(alias) default null,
-- 
--     prev_id     decimal         references authority(id) default null,
--     prev_alias  varchar(1024)   references authority(alias) default null,
    
    level       decimal default 0,
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
);


/**
 *  Официальный статус пользователя: по сути должность
**/
create sequence seq_ostatus_id;
create table ostatus(
    id          decimal primary key default nextval('seq_ostatus_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
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
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
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
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
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
--     isdeleted     boolean default false
-- );
-- -- ----------------------------------------------------------------------
-- --




------------------------------------------------------------------------------
-- Географические страны
------------------------------------------------------------------------------

-- create sequence seq_pregion_id;
-- create table pregion(
--     id          decimal primary key default nextval('seq_perspicbody_id'),
--     name_ti     decimal unique default nextval('seq_any_ti'),
--     alias       varchar(1024),
--     pregion_id  decimal references pregion(id)     default null,
--     created     timestamp without time zone not null default utcnow(),
--     isdeleted   boolean default false,
--     constraint  pregion_alias_pregion_id_many_key unique (alias,pregion_id)
-- );

/**
 *  Города реального мира
**/

create sequence seq_geo_id;
create table geo(
    id          decimal primary key default nextval('seq_geo_id'),
    alias       varchar(1024)   default null,
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    /**
        Номер языковой сущности
    **/
    descr_ti    decimal unique      default nextval('seq_any_ti'),
    -- alias       varchar(1024)   unique,
    /**
        Родительский элемент
    **/
    parent_id    decimal references geo(id) default null,

    /**
        целевых ссылок на эту сущность
    **/
    nchildtargets   decimal default 0,

    /**
        целевых ссылок на эту сущность и ее детей
    **/
    nnodetargets    decimal default 0,
    /**
        количество детей (дочерних элементов)
    **/
    nchildren       decimal default 0,
    /**
        количество вершин в кусте
    **/
    nnodes          decimal default 0,
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
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
    login               varchar(1024) default 'user_'
                        || CAST (nextval('seq_pers_fakelogin')
                            as varchar(1024))
                        || '_'
                        || extract(epoch from now())
                        || '_' not null unique,
    nick                varchar(1024) default 'user_'
                        || CAST (nextval('seq_pers_fakelogin')
                            as varchar(1024))
                        || '_'
                        || extract(epoch from now())
                        || '_' not null unique,
    -- nick        varchar(1024)   not null unique,
    phash       char(32)        not null,
    email       varchar(1024)   unique default null,
    phone       numeric         default null,
    /**
        ------------------------------------------------------------
            Информация о пользователе
        ------------------------------------------------------------
    **/
    fname       varchar(1024)   default null,
    sname       varchar(1024)   default null,
    isempl      boolean         default null,
    empl        varchar(1024)   default null,

    hobby       varchar(1024)   default null,
    descr       varchar(1024)   default null,
    interest    varchar(1024)   default null,
    

    geo_id  decimal references  geo(id)     default null,

    
    birthday    timestamp       without time zone default null,
    -- gender_id           decimal references gender(id)      default null,
    ismale      boolean    default true,
    /**
        ------------------------------------------------------------
            Информация о персоонаже
        ------------------------------------------------------------
    **/
    money               numeric(1000, 2)    default 300.0,
    /**
        Статус online \ offline
    **/
    pstatus_id          decimal         references pstatus(id),
    pstatus_alias       varchar(1024)   references pstatus(alias),

    /**
        Чиновничий статус пользователя
    **/
    isostatusable       boolean         default true,
    ostatus_id          decimal         references ostatus(id)    default null,
    ostatus_alias       varchar(1024)   references ostatus(alias) default null,

    /**
        Авторитет пользователя
    **/
    authority_id        decimal         references authority(id)        default null,
    authority_alias     varchar(1024)   references authority(alias)     default null,
    authority_level     numeric         default null,
    /**
        Опыт пользователя,
        некоторая непрерывная велицина.
    **/
    exper               numeric         default 0,
    /**
        Недостаток опыта.
        Сколько не хватает для перехода на следующий уровень.
    **/
    experlack           numeric         default null,

    /**
        Стоймость недостатока опыта.
        Сколько стоит то, что не хватает для перехода на следующий уровень.
    **/
    experlackprice      numeric(1000, 2)         default null,
    
    /**
        Эмоции пользователя
    **/
    emotion_id          decimal         references emotion(id)     default null,
    emotion_alias       varchar(1024)   references emotion(alias)  default null,
    /**
        Семейное положения пользователя
    **/
    mstatus_id          decimal         references mstatus(id)     default null,
    mstatus_alias       varchar(1024)   references mstatus(alias)  default null,
    /**
        язык пользователяад
    **/
    lang_id             decimal         references lang(id)     default null,
    lang_alias          varchar(1024)   references lang(alias)  default null,

    married_id          decimal references pers(id)        default null,
    mother_id           decimal references pers(id)        default null,
    father_id           decimal references pers(id)        default null,
    /** Общество в котором он состоит
        [см далее]: live_community_id decimal references community(id) default null,
    **/
    live_community_head         varchar(1024) /*references doc(head)*/ default null,
    live_community_approved     boolean  default null,
    live_community_rejectreason text default null,
    
    /** Общество которое он создал
        [см далее]: own_community_id decimal references community(id) default null,
    **/
    own_community_head      varchar(1024) /*references doc(head)*/ default null,
    
    /** Страна \ рай \ aд, где он сейчас находится
        [см далее]: live_room_id decimal references room(id) default null,
    **/ 
    live_room_head          varchar(1024) /*references doc(head)*/ default null,
    live_room_approved      boolean  default true,

    /** Страна \ рай \ aд, гражданином которой он является
        [см далее]: citizen_room_id decimal references room(id) default null,
    **/ 
    citizen_room_head   varchar(1024) /*references doc(head)*/ default null,
    
    citizen_room_fromdatetime       timestamp without time zone default null,
      
    /**
        позиция в списке
    **/
    live_room_pos       numeric default 0,

    
    /** Страна \ рай \ aд
        [см далее]: own_room_id decimal references room(id) default null,
    **/
    
    own_room_head       varchar(1024) /*references doc(head)*/ default null,
    
    -- allowauctoffer      boolean default false,

    perspichead_id      decimal references perspichead(id)   default null,
    perspicbody_id      decimal references perspicbody(id)   default null,


    show_money_acctype_id     decimal         references acctype(id)    default null,
    show_money_acctype_alias  varchar(1024)   references acctype(alias) default null,

    get_message_acctype_id     decimal         references acctype(id)    default null,
    get_message_acctype_alias  varchar(1024)   references acctype(alias) default null,

    get_thingbuy_acctype_id     decimal         references acctype(id)    default null,
    get_thingbuy_acctype_alias  varchar(1024)   references acctype(alias) default null,

    /**
        ------------------------------------------------------------
            Внутрениие поля
        ------------------------------------------------------------
    **/

    /**
        позиция в списке
    **/
    position            numeric default null,
    /**
        дата создания
    **/
    created             timestamp without time zone not null default utcnow(),
    /**
        дата последний модификации
    **/
    updated             timestamp without time zone not null default utcnow(),
    /**
        количество просмотров
    **/
    vcounter            decimal default null,
    /**
        количество обновлений
    **/
    nupdates            decimal default 0,

    /**
        флаг удаления по времени
    **/
    istimeover          boolean default false,
    
    /**
        флаг удаления
    **/    
    isdeleted           boolean default false
);

alter table file add column owner_id
    decimal references pers(id) default null;

alter table fileinfo add owner_id
    decimal references pers(id) default null;


alter table file     add owner_nick varchar(1024)
    default null default null;
alter table fileinfo add owner_nick varchar(1024)
    default null default null;


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
    issystem        boolean    default false,
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean    default false
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
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean    default false
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
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean    default false
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
 *  Типы связей между пользователями
**/
create sequence seq_friendtype_id;
create table friendtype (
    id          decimal primary key default nextval('seq_friendtype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean    default false
);


/**
 *  Друзья
**/
create sequence seq_friend_id;
create table friend(
    id                  decimal primary key default     nextval('seq_friend_id'),

    pers_id             decimal         references pers(id)             default null,
    pers_nick           varchar(1024)                                   default null,
    friend_id           decimal         references pers(id)             default null,
    friend_nick         varchar(1024)                                   default null,
    friendtype_id       decimal         references friendtype(id)       default null,
    friendtype_alias    varchar(1024)   references friendtype(alias)    default null,
    
    created             timestamp       without time zone not null      default utcnow(),
    constraint          friend_pers_id_friend_id_many_key unique (pers_id, friend_id)
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
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
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
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
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
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
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
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
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
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
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
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
);


/**
 *  Документ
**/
create sequence seq_doc_id;
create table doc(
    id                  decimal primary key default     nextval('seq_doc_id'),
    head                text,
    body                text default null,
    head_ti             decimal unique default nextval('seq_any_ti');
    body_ti             decimal unique default nextval('seq_any_ti');

    /**
        Владелец документа
    **/
    owner_id            decimal         references pers(id)     default null,
    owner_nick          varchar(1024)                           default null,

    /**
        Непросмотрен, разрешен, запрещен, там где это нужно,
    **/
    oktype_id           decimal         references oktype(id)     default null,
    oktype_alias        varchar(1024)   references oktype(alias)  default null,
    /**
        Тип документа: блог, коммент к блогу, галерея,
            фото, коммент к фото, attach descr.
    **/
    doctype_id          decimal         references doctype(id)    default null,
    doctype_alias       varchar(1024)   references doctype(alias) default null,
    
    /**
        Типы контента: Обычный, эротический
    **/
    contype_id          decimal         references contype(id)    default null,
    contype_alias       varchar(1024)   references contype(alias) default null,
    /**
        Разрешение на чтение
    **/
    read_acctype_id     decimal         references acctype(id)    default null,
    read_acctype_alias  varchar(1024)   references acctype(alias) default null,
    /**
        Разрешение комментов
    **/
    comm_acctype_id     decimal         references acctype(id)    default null,
    comm_acctype_alias  varchar(1024)   references acctype(alias) default null,


    orig_id           decimal         references doc(id)      default null,
    orig_owner_id     decimal         references pers(id)     default null,
    orig_owner_nick   varchar(1024)                           default null,


    /**
        Родительский элемент
    **/
    parent_id           decimal         references doc(id)        default null,
    /**
        Оповещение комментов
    **/
    isnoticeable          boolean default false,
    /**
        количество детей (дочерних элементов)
    **/
    nchildren           decimal default 0,
    /**
        количество вершин в кусте
    **/
    nnodes              decimal default 0,
    /**
        позиция в списке
    **/
    position            numeric default null,
    /**
        дата создания
    **/
    created             timestamp without time zone not null default utcnow(),
    /**
        дата последний модификации
    **/
    updated             timestamp without time zone not null default utcnow(),
    /**
        количество просмотров документа
    **/
    nviews              decimal default 0,
    /**
        количество голосований
    **/
    nvotes              decimal default 0,
    /**
        количество обновлений
    **/
    nupdates            decimal default 0,

    isrepost            boolean default false,
    isrepostcont        boolean default false,
    isrepostable        boolean default true,
    
    /**
        флаг удаления
    **/
    isdeleted           boolean default false
);


alter table fileinfo add column doc_id
    decimal references doc(id) default null;
alter table file add column doc_id
    decimal references doc(id) default null;

------------------------------------------------------------------------------
-- Аттачи
------------------------------------------------------------------------------

create sequence seq_attachtype_id;
create table attachtype(
    id              decimal primary key default     nextval('seq_attachtype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti           decimal unique default nextval('seq_any_ti'),
    alias             varchar(1024)   unique,
    created           timestamp without time zone not null default utcnow(),
    isdeleted         boolean default false
);

create table attach(
    doc_id          decimal unique references doc(id)   default null,
    attachtype_id      decimal         references attachtype(id)    default null,
    attachtype_alias   varchar(1024)                                default null,
    file_id         decimal references file(id)         default null
);

------------------------------------------------------------------------------
-- Репост
------------------------------------------------------------------------------


create sequence seq_repost_id;    
create table repost(
    id                decimal   primary key default nextval('seq_repost_id'),
    doc_id            decimal         references doc(id)      default null,
    owner_id          decimal         references pers(id)     default null,
    owner_nick        varchar(1024)                           default null,
    orig_doc_id       decimal         references doc(id)      default null,
    orig_owner_id     decimal         references pers(id)     default null,
    orig_owner_nick   varchar(1024)                           default null,
    created           timestamp without time zone not null default utcnow(),
    isdeleted         boolean default false
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
    nposts              decimal default 0,
    npublicposts        decimal default 0,
    nprivateposts       decimal default 0,
    nprotectedposts     decimal default 0,
    ncomments           decimal default 0
);

/**
 *  Запись блога 
**/
create table post(
    doc_id              decimal unique references doc(id),
    pic_file_id         decimal references file(id)     default null,
    ncomments           decimal default 0
);


/**
 *  Состояние жалобы
**/
create sequence seq_claimtype_id;
create table claimtype(
    id          decimal primary key default nextval('seq_claimtype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
);


create table claim(
    doc_id              decimal unique references doc(id),
    pers_id             decimal         references pers(id),
    pers_nick           varchar(1024)   default null,
    judge_id            decimal         references pers(id),
    judge_nick          varchar(1024)   default null,
    claimtype_id        decimal
        references claimtype(id)        default null,
    claimtype_alias     varchar(1024)
        references claimtype(alias)     default null
);


/**
 *  Запись комментарий
**/
create table comment(
    doc_id              decimal unique references doc(id),
    ncomments           decimal default 0
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
create table album(
    doc_id              decimal unique references doc(id),
    ncomments           decimal default 0,
    /**
        Разрешение на перепост
    **/
    repost              boolean default false
);

create table photo(
    doc_id              decimal unique references doc(id),
    ncomments           decimal default 0,
    file_id             decimal references file(id)     default null,
    file_path           varchar(1024)   default null,
    /**
        Разрешение на перепост
    **/
    is_cover            boolean default false
);


create table wall(
    doc_id      decimal unique references doc(id),
    file_id     decimal references file(id)     default null,
);

create table back(
    doc_id      decimal unique references doc(id),
    file_id     decimal references file(id)     default null,
);

create table flag(
    doc_id      decimal unique references doc(id),
    file_id     decimal references file(id)     default null,
);

create table arms(
    doc_id      decimal unique references doc(id),
    file_id     decimal references file(id)     default null,
);

create table perspichead(
    doc_id      decimal unique references doc(id),
    x           decimal default null,
    y           decimal default null,
    file_id     decimal references file(id)     default null
);


create table perspicbody(
    doc_id      decimal unique references doc(id),
    x           decimal default null,
    y           decimal default null,
    file_id     decimal references file(id)     default null
);


create sequence seq_vote_id;
create table vote(
    id                  decimal primary key default nextval('seq_vote_id'),
    doc_id              decimal         references doc(id),
    pers_id             decimal         references pers(id),
    pers_nick           varchar(1024)   default null,
    rating              numeric         default null,
    created             timestamp without time zone not null default utcnow(),
    isdeleted           boolean default false,
    constraint          room2topic_doc_id_pers_id_many_key    unique (doc_id, pers_id)
);

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
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
);



alter table pers add column live_roomtype_id
    decimal references roomtype(id)     default null;

alter table pers add column live_roomtype_alias
    varchar(1024) references roomtype(alias)     default null;

alter table pers add column isprisoner
    boolean default false;

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
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
);

/**
 *  Дерево тем чата. Редактируется администраторами и пользователями. 
**/
create sequence seq_topic_id;
create table topic(
    id          decimal primary key default nextval('seq_topic_id'),
    alias       varchar(1024)   unique,
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    /**
        Номер языковой сущности
    **/
    descr_ti    decimal unique      default nextval('seq_any_ti'),
    -- alias       varchar(1024)   unique,
    /**
        Родительский элемент
    **/
    parent_id    decimal references topic(id) default null,

    /**
        целевых ссылок на эту сущность
    **/
    nchildtargets   decimal default 0,

    /**
        ссылок на эту сущность из комнаты
    **/
    nroomtargets   decimal default 0,


    /**
        ссылок на эту сущность из сообществ
    **/
    ncommunitytargets   decimal default 0,

    
    /**
        целевых ссылок на эту сущность и ее детей
    **/
    nnodetargets    decimal default 0,
    /**
        количество детей (дочерних элементов)
    **/
    nchildren       decimal default 0,
    /**
        количество вершин в кусте
    **/
    nnodes          decimal default 0,
    created         timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
);


create sequence seq_regimen_id;
create table regimen(
    id          decimal primary key default nextval('seq_regimen_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    descr_ti    decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
);



create table room(
    doc_id              decimal unique references doc(id),
    ulimit              decimal default 5000,
    /**
        Тип комнаты
    **/
    roomtype_id         decimal         references roomtype(id)     default null,
    roomtype_alias      varchar(1024)   references roomtype(alias)  default null,
    /**
        Язык комнаты
    **/
    chatlang_id         decimal         references chatlang(id)     default null,
    chatlang_alias      varchar(1024)   references chatlang(alias)  default null,
    
    /**
        Фон комнаты
    **/
    back_file_id        decimal references file(id)     default null,
    back_path           varchar(1024)                   default null,
    
    /**
        Обои комнаты
    **/
    wall_file_id        decimal references file(id)     default null,
    wall_path           varchar(1024)                   default null,
    
    /**
        Флаг комнаты
    **/
    flag_file_id        decimal references file(id)     default null,
    flag_path           varchar(1024)                   default null,
    
    /**
        Герб комнаты
    **/
    arms_file_id        decimal references file(id)     default null,
    arms_path           varchar(1024)                   default null,
    
    /**
        Режим комнаты
    **/
    regimen_id          decimal         references regimen(id)      default null,
    regimen_alias       varchar(1024)   references regimen(alias)   default null,
    topic_id            decimal references topic(id) default null,
    slogan              text default null,
    weather             text default null,
    /**
        Авторитет пользователя
    **/
    authority_id        decimal         references authority(id)        default null,
    authority_alias     varchar(1024)   references authority(alias)     default null,
    /**
        Опыт пользователя,
        некоторая непрерывная велицина.
    **/
    exper               numeric         default 0,
    /**
        Недостаток опыта.
        Сколько не хватает для перехода на следующий уровень.
    **/
    experlack           numeric         default null,

    /**
        Стоймость недостатока опыта.
        Сколько стоит то, что не хватает для перехода на следующий уровень.
    **/
    experlackprice      numeric(1000, 2)         default null,
    treas               numeric(1000, 2) default 1

    
    

    
--     bearing - герб
--     flag - ссылка на картинку флага
--     wallpaper - ссылка на картинку фона ?????? не закончено
);


alter table pers add  column live_room_id
    decimal references room(doc_id) default null;

alter table pers add  column citizen_room_id
    decimal references room(doc_id) default null;

alter table pers add  column own_room_id
    decimal references room(doc_id) default null;


create sequence seq_roomlisttype_id;
create table roomlisttype(
    id          decimal primary key default nextval('seq_roomlisttype_id'),
    name_ti     decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
);

create sequence seq_roomlist_id;
create table roomlist(
    id          decimal primary key default nextval('seq_roomlist_id'),
    owner_id    decimal        references pers(id)       default null,
    owner_nick  varchar(1024)                            default null,

    room_id     decimal         references room(doc_id)  default null,
    room_head   varchar(1024)                            default null,

    roomlisttype_id        decimal         references roomlisttype(id)        default null,
    roomlisttype_alias     varchar(1024)   references roomlisttype(alias)     default null,
    
    text default null,
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
);


------------------------------------------------------------------------------
-- Сообщество
------------------------------------------------------------------------------

/**
 *  Типы сообществ (обычные, тайные, элитные)
**/
create sequence seq_communitytype_id;
create table communitytype(
    id          decimal primary key default nextval('seq_communitytype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp without time zone not null default utcnow(),
    isdeleted   boolean default false
);

/**
 *  Сообщество
**/
create table community(
    doc_id                  decimal unique references doc(id),
    /**
        Тип сообщества
    **/
    communitytype_id        decimal         references communitytype(id)    default null,
    communitytype_alias     varchar(1024)   references communitytype(alias) default null,

    read_gte_authority_id             decimal           references authority(id) default null,
    read_gte_authority_alias          varchar(1024)     references authority(alias) default null,
    read_gte_authority_level          numeric           default null,

    cands_gte_authority_id            decimal           references authority(id) default null,
    cands_gte_authority_alias         varchar(1024)     references authority(alias) default null,
    cands_gte_authority_level         numeric           default null,


    /**
        Фон сообщества
    **/
    back_file_id        decimal references file(id)     default null,
    back_path           varchar(1024)                   default null,

    /**
        Обои сообщества
    **/
    wall_file_id        decimal references file(id)     default null,
    wall_path           varchar(1024)                   default null,

    /**
        Флаг сообщества
    **/
    flag_file_id        decimal references file(id)     default null,
    flag_path           varchar(1024)                   default null,

    /**
        Герб сообщества
    **/
    arms_file_id        decimal references file(id)     default null,
    arms_path           varchar(1024)                   default null,

    isclosed                boolean default false,
    ncands                  decimal default 0,
    nmembs                  decimal default 0,
    slogan                  text default null,
    treas                   decimal default 0,
    fee                     decimal default 0
);

create sequence seq_communityhisttype_id;
create table communityhisttype(
    id          decimal primary key default nextval('seq_communityhisttype_id'),
    name_ti     decimal unique  default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp       without time zone not null default utcnow(),
    isdeleted   boolean         default false
);


/**
 *  Кандидат в сообщество
**/
create sequence seq_communityhist_id;
create table communityhist(
    id                          decimal         primary key default nextval('seq_communityhist_id'),
    pers_id                     decimal         references pers(id) not null,
    pers_nick                   varchar(1024)   default null,
    community_id                decimal         references community(doc_id)  default null,
    communityhisttype_id        decimal         references communityhisttype(id)    default null,
    communityhisttype_alias     varchar(1024)   references communityhisttype(alias) default null,

    created             timestamp without time zone not null default utcnow(),
    isdeleted           boolean default false
);


/**
 *  Кандидат в сообщество
**/
create sequence seq_communitycand_id;
create table communitycand (
    id                  decimal primary key default nextval('seq_communitycand_id'),
    pers_id             decimal references pers(id) not null,
    community_id        decimal references community(doc_id) not null,
    created             timestamp without time zone not null default utcnow(),
    isdeleted           boolean default false
);

/**
 *  Член сообщетва
**/
create sequence seq_communitymemb_id;
create table communitymemb (
    id                  decimal primary key default nextval('seq_communitymemb_id'),
    pers_id             decimal references pers(id) not null,
    community_id        decimal references community(doc_id) not null,
    created             timestamp without time zone not null default utcnow(),
    isdeleted           boolean default false
);

alter table pers add column live_community_id
    decimal references community(doc_id) default null;

alter table pers add column own_community_id
    decimal references community(doc_id) default null;

/**
 *  Календарная запись
**/

create sequence seq_noticetype_id;
create table noticetype(
    id          decimal primary key default nextval('seq_noticetype_id'),
    name_ti     decimal unique  default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    created     timestamp       without time zone not null default utcnow(),
    isdeleted   boolean            default false
);

create table notice(
    doc_id              decimal unique references doc(id),
    noticetype_id       decimal references noticetype(id)       default null,
    noticetype_alias    varchar(1024)   references noticetype(alias)    default null,
    datetime            timestamp without time zone             default utcnow()
);

alter table notice add column pers_id   decimal
    references pers(id) default null;
alter table notice add column pers_nick varchar(1024)
    references pers(nick) default null;


create sequence seq_action_id;
create table action(
    id                  decimal primary key default nextval('seq_action_id'),
    actiontype_id       decimal references actiontype(id)       default null,
    actiontype_alias    decimal references actiontype(alias)    default null,
    pers_id             decimal references pers(id)             default null,
    pers_nick           varchar(1024)                           default null,
    owner_id    decimal references pers(id)            default null,
    owner_nick  varchar(1024)                          default null,
    ispaid      boolean         default false,
    price       numeric(1000, 2)          default 0,
    created     timestamp       without time zone not null
        default utcnow(),
    expired     timestamp without time zone not null
        default utcnow() + interval '1 week',
    isdeleted   boolean         default false
);


------------------------------------------------------------------------------
-- События
------------------------------------------------------------------------------
    
create sequence seq_eventtype_id;
create table eventtype(
    id          decimal primary key default nextval('seq_eventtype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique  default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    isnews      boolean  default false,
    created     timestamp       without time zone not null default utcnow(),
    isdeleted   boolean            default false
);


create table event(
    doc_id              decimal unique references doc(id),
    eventtype_id        decimal         references eventtype(id)    default null,
    eventtype_alias     varchar(1024)   references eventtype(alias) default null,
    pers_id             decimal references pers(id) default null,
    pers_nick           varchar(1024)               default null
);


------------------------------------------------------------------------------
-- Сообщения
------------------------------------------------------------------------------

/**
    Что это такое, не очень понятно.
**/
create sequence seq_messagetype_id;
create table messagetype(
    id          decimal primary key default nextval('seq_messagetype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti     decimal unique      default nextval('seq_any_ti'),
    alias       varchar(1024)   unique,
    isdeleted   boolean default false
);


create table message(
    doc_id              decimal unique references doc(id),
    /**
        Тип сообщения (пока не понятно, что это)
    **/
    messagetype_id      decimal         references messagetype(id)      default null,
    messagetype_alias   varchar(1024)   references messagetype(alias)   default null,
    
    reader_id           decimal         references pers(id) default null,
    reader_nick         varchar(1024)   default null default null,
    /**
        Удалено для отправителя (из почтового ящика отправителя)
    **/
    isdfo               boolean default false,
    /**
        Удалено для получателя (из почтового ящика получателя)
    **/
    isdfr               boolean default false
);

------------------------------------------------------------------------------
-- Лот аукциона
-- Проводим аукцион только для страны.
------------------------------------------------------------------------------

create table roomlot(
    doc_id          decimal unique references doc(id),
    room_id         decimal references room(doc_id)        default null,
    room_head       varchar(1024) /*references doc(head)*/ default null,
    dtstart         timestamp without time zone not null default utcnow(),
    dtstop          timestamp without time zone not null default utcnow() + interval '1 week',
    betmin          numeric(1000, 2) default 0,
    betcur          numeric(1000, 2) default 0,
    betmax          numeric(1000, 2) default 100
);


-- create table roomlot_log(
--     doc_id          decimal unique references doc(id),
--     room_id         decimal references room(doc_id)        default null,
--     room_head       varchar(1024) /*references doc(head)*/ default null,
--     dtstart         timestamp without time zone not null default utcnow(),
--     dtstop          timestamp without time zone not null default utcnow() + interval '1 week',
--     betmin          numeric(1000, 2) default 0,
--     betmax          numeric(1000, 2) default 100,
-- );

------------------------------------------------------------------------------
-- Ставки в аукционе.
------------------------------------------------------------------------------

create sequence seq_roombet_id;
create table roombet(
    id              decimal primary key default nextval('seq_roombet_id'),

    roomlot_id      decimal         references roomlot(doc_id)      default null,
    room_id         decimal        /*  references roomlot(room_id)  */ default null,
    room_head       varchar(1024)  /* references roomlot(room_head) */ default null,

    /**
        Владелец, тот кто обладает товаром после покупки
    **/
    owner_id        decimal             references pers(id)     not null,
    owner_nick      varchar(1024)       default null   not null,
    price           numeric(1000, 2)    default 0,

    created         timestamp without time zone not null    default utcnow(),
    isdeleted       boolean    default false
);


alter table room add column roomlot_id          decimal         references roomlot(doc_id)  default null;
alter table room add column roomlot_betmin      numeric(1000, 2)                            default null;
alter table room add column roomlot_betmax      numeric(1000, 2)                            default null;
alter table room add column roomlot_dtstart     timestamp without time zone not null default utcnow();
alter table room add column roomlot_dtstop      timestamp without time zone not null default utcnow() + interval '1 week';
alter table room add column roombet_id          decimal        references roombet(id)       default null;
alter table room add column roombet_owner_id    decimal        references pers(id)          default null;
alter table room add column roombet_owner_nick  varchar(1024)       default null   default null;
alter table room add column roombet_price       decimal                                     default null;


/*
create sequence seq_roomoffer_id;
create table roomoffer(
    id              decimal primary key default nextval('seq_roomoffer_id'),

    roomlot_id      decimal references roomlot(doc_id)      default null,
    room_id         decimal references room(doc_id)         default null,
    room_head       varchar(1024) references doc(head) default nul

    owner_id        decimal         references pers(id)     not null,
    owner_nick      varchar(1024)   default null   not null,

    reader_id       decimal         references pers(id)     not null,
    reader_nick     varchar(1024)   default null   not null,

    price           numeric(1000, 2) default 0,

    created         timestamp without time zone not null    default utcnow(),
    isdeleted       boolean    default false
);

*/



/****************************************************************************
    =====================================================================
                                ВЕЩИ-ПОКУПКИ
    =====================================================================
****************************************************************************/

create sequence seq_thingtype_id;
create table thingtype(
    id              decimal primary key default nextval('seq_thingtype_id'),
    alias           varchar(1024)   unique,
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique      default nextval('seq_any_ti'),
    /**
        Номер языковой сущности
    **/
    descr_ti        decimal unique      default nextval('seq_any_ti'),
    /**
        Родительский элемент
    **/
    parent_id       decimal references thingtype(id) default null,

    /**
        целевых ссылок на эту сущность
    **/
    nchildtargets   decimal default 0,
    /**
        целевых ссылок на эту сущность и ее детей
    **/
    nnodetargets    decimal default 0,
    /**
        количество детей (дочерних элементов)
    **/
    nchildren           decimal default 0,
    /**
        количество вершин в кусте
    **/
    nnodes              decimal default 0,

    file_id         references file(id) default null,
    
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean    default false
);


create sequence seq_thing_id;
create sequence seq_thing_alias;
create table thing(
    id              decimal primary key default nextval('seq_thing_id'),
    alias           varchar(1024) default '_'
                        || CAST (nextval('seq_thing_alias')
                            as varchar(1024))
                        || '_'
                        || extract(epoch from now())
                        || '_' not null unique,
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique      default nextval('seq_any_ti'),
    /**
        Номер языковой сущности
    **/
    descr_ti        decimal unique      default nextval('seq_any_ti'),
    
    /**
        Ccылка на вершину дерева типов вещей
    **/
    thingtype_id       decimal references thingtype(id) default null,
    thingtype_alias    varchar(1024)                    default null,

    price           numeric(1000, 2)   default null,
    rent            numeric(1000, 2)   default null,
    file_id         references file(id) default null,

    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean    default false
);




create sequence seq_renttype_id;
create table renttype(
    id              decimal primary key default nextval('seq_renttype_id'),
    alias           varchar(1024)   unique,
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique      default nextval('seq_any_ti'),
    /**
        Номер языковой сущности
    **/
    descr_ti        decimal unique      default nextval('seq_any_ti'),
    /**
        Родительский элемент
    **/
    parent_id       decimal references renttype(id) default null,

    /**
        целевых ссылок на эту сущность
    **/
    nchildtargets   decimal default 0,
    /**
        целевых ссылок на эту сущность и ее детей
    **/
    nnodetargets    decimal default 0,
    /**
        количество детей (дочерних элементов)
    **/
    nchildren           decimal default 0,
    /**
        количество вершин в кусте
    **/
    nnodes              decimal default 0,

    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean    default false
);


create sequence seq_rent_id;
create sequence seq_rent_alias;
create table rent(
    id              decimal primary key default nextval('seq_rent_id'),
    alias           varchar(1024) default '_'
                        || CAST (nextval('seq_rent_alias')
                            as varchar(1024))
                        || '_'
                        || extract(epoch from now())
                        || '_' not null unique,
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique      default nextval('seq_any_ti'),
    /**
        Номер языковой сущности
    **/
    descr_ti        decimal unique      default nextval('seq_any_ti'),

    /**
        Ccылка на вершину дерева типов вещей
    **/
    renttype_id    decimal references renttype(id) default null,

    price           numeric(1000, 2)   default null,

    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean    default false
);


/**
 *  Многие ко многим для пользователей и вещей
**/
-- create sequence seq_buytype_id;
-- create table buytype (
--     id                  decimal primary key default nextval('seq_buytype_id'),
--     /**
--         Номер языковой сущности
--     **/
--     name_ti     decimal unique      default nextval('seq_any_ti'),
--     alias       varchar(1024)   unique,
--     isdeleted   boolean default false
-- );

/**
 *  Многие ко многим для пользователей и вещей
**/
create sequence seq_thingbuy_id;
create table thingbuy (
    id                  decimal primary key default nextval('seq_thingbuy_id'),

    /**
        Покупатель, тот кто платит
    **/
    buyer_id            decimal         references pers(id)     not null,
    buyer_nick          varchar(1024)   default null   not null,

    /**
        Владелец, тот кто обладает товаром после покупки
    **/
    owner_id            decimal         references pers(id)     not null,
    owner_nick          varchar(1024)   default null   not null,

    /**
        Вещь которую приобрели
    **/
    thing_id            decimal         references thing(id)    not null,
    thing_alias         varchar(1024)   references thing(alias) not null,

    price               numeric(1000, 2) default null,
    rent                numeric(1000, 2) default null,
    file_id             references file(id) default null,

    counter             timestamp without time zone          default utcnow(),
    expired             timestamp without time zone          default null,

    created             timestamp without time zone not null default utcnow(),
    isdeleted           boolean default false
);



/**
 *  Многие ко многим для пользователей и вещей
**/
create sequence seq_thingwish_id;
create table thingwish (
    id                  decimal primary key default nextval('seq_thingwish_id'),

    /**
        Владелец, тот кто обладает товаром после покупки
    **/
    owner_id            decimal         references pers(id)     not null,
    owner_nick          varchar(1024)   default null   not null,

    /**
        Вещь которую приобрели
    **/
    thing_id            decimal         references thing(id)    not null,
    thing_alias         varchar(1024)   references thing(alias) not null,

    price               numeric(1000, 2) default null,
    rent                numeric(1000, 2) default null,
    
    counter             timestamp without time zone not null default utcnow(),

    -- expired             timestamp without time zone          default null,

    created             timestamp without time zone not null default utcnow(),
    isdeleted           boolean default false
);


create sequence seq_experbuy_id;
create table experbuy (
    id                  decimal primary key default nextval('seq_experbuy_id'),

    /**
        Покупатель, тот кто платит
    **/
    buyer_id            decimal         references pers(id)     not null,
    buyer_nick          varchar(1024)   default null   not null,

    /**
        Владелец, тот кто обладает товаром после покупки
    **/
    owner_id            decimal         references pers(id)     not null,
    owner_nick          varchar(1024)   default null   not null,

    /**
        Вещь которую приобрели --- опыт.
        Нужно знать, какой количество
    **/
    exper               numeric             default null,
    price               numeric(1000, 2)    default null,

    created             timestamp without time zone not null default utcnow(),
    isdeleted           boolean default false
);



create sequence seq_roomexperbuy_id;
create table roomexperbuy (
    id                  decimal primary key default nextval('seq_roomexperbuy_id'),
    /**
        Покупатель, тот кто платит
    **/
    room_id            decimal         references room(id)      default null,
    room_head          varchar(1024)                            default null,
    
    /**
        Вещь которую приобрели --- опыт.
        Нужно знать, какой количество
    **/
    exper               numeric             default null,
    price               numeric(1000, 2)    default null,

    created             timestamp without time zone not null default utcnow(),
    isdeleted           boolean default false
);


-- create sequence seq_incometype_id;
-- create table incometype (
--     id          decimal primary key default nextval('seq_eventtype_id'),
--     /**
--         Номер языковой сущности
--     **/
--     name_ti     decimal unique      default nextval('seq_any_ti'),
--     alias       varchar(1024)   unique,
--     isdeleted   boolean default false
-- );


create sequence seq_transtype_id;
create table transtype (
    id              decimal primary key default nextval('seq_eventtype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique      default nextval('seq_any_ti'),
    isincome        boolean                default null,
    alias           varchar(1024)   unique,
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
);

create sequence seq_rptrans_id;
create table rptrans (
    id                 decimal primary key default nextval('seq_rptrans_id'),
    pers_id            decimal         references pers(id)     not null,
    pers_nick          varchar(1024)   default null   not null,
    room_id            decimal         references room(doc_id)       not null,
    transtype_id       decimal         references transtype(id)      default null,
    transtype_alias    varchar(1024)   references transtype(alias)   default null,
    price              numeric(1000, 2)    default null,
    created            timestamp without time zone not null default utcnow(),
    isdeleted          boolean default false
);


create sequence seq_cptrans_id;
create table cptrans (
    id                 decimal primary key default nextval('seq_cptrans_id'),
    pers_id            decimal         references pers(id)     not null,
    pers_nick          varchar(1024)   default null   not null,
    community_id       decimal         references community(doc_id)       not null,
    transtype_id       decimal         references transtype(id)      default null,
    transtype_alias    varchar(1024)   references transtype(alias)   default null,
    price              numeric(1000, 2)    default null,
    created            timestamp without time zone not null default utcnow(),
    isdeleted          boolean default false
);



create sequence seq_treastype_id;
create table treastype (
    id          decimal primary key default nextval('seq_eventtype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti         decimal unique      default nextval('seq_any_ti'),
    isincome        boolean                default null,
    alias           varchar(1024)   unique,
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
);

create sequence seq_roomtreas_id;
create table roomtreas (
    id                  decimal primary key default nextval('seq_roomtreas_id'),

    /**
        Покупатель, тот кто платит
    **/
    pers_id            decimal         references pers(id)     not null,
    pers_nick          varchar(1024)   default null   not null,

    /**
        Владелец, тот кто обладает товаром после покупки
    **/
    room_id            decimal         references room(doc_id)       not null,
    treastype_id       decimal         references treastype(id)      default null,
    treastype_alias    varchar(1024)   references treastype(alias)   default null,
    isincome           boolean            default null,
    price              numeric(1000, 2)    default null,
    info               varchar(1024)   default null,
    created            timestamp without time zone not null default utcnow(),
    isdeleted          boolean default false
);



create sequence seq_communitytreas_id;
create table communitytreas (
    id                  decimal primary key default nextval('seq_communitytreas_id'),

    /**
        Покупатель, тот кто платит
    **/
    pers_id            decimal         references pers(id)     not null,
    pers_nick          varchar(1024)   default null   not null,

    /**
        Владелец, тот кто обладает товаром после покупки
    **/
    community_id       decimal         references community(doc_id)       not null,
    treastype_id       decimal         references treastype(id)      default null,
    treastype_alias    varchar(1024)   references treastype(alias)   default null,
    isincome           boolean            default null,
    price              numeric(1000, 2)    default null,
    info               varchar(1024)   default null,
    created            timestamp without time zone not null default utcnow(),
    isdeleted          boolean default false
);


create sequence seq_paytype_id;
create table paytype (
    id              decimal primary key default     nextval('seq_paytype_id'),
    /**
        Номер языковой сущности
    **/
    name_ti                 decimal unique default nextval('seq_any_ti'),
    alias                   varchar(1024)   unique,

    isincome                boolean default null,

    created                 timestamp without time zone not null default utcnow(),
    isdeleted               boolean default false
);


create sequence seq_pay_id;
create table pay (
    id              decimal primary key default nextval('seq_pay_id'),

    isincome                boolean default null,
    price           numeric(1000, 2)    default 0,
    paytype_id      decimal         references paytype(id)      not null,
    paytype_alias   varchar(1024)   references paytype(alias)   not null,
    pers_id         decimal         references pers(id)         not null,
    pers_nick       varchar(1024)   default null       not null,
    info            varchar(1024)   default null,
    created         timestamp without time zone not null default utcnow(),
    isdeleted       boolean default false
);





create sequence seq_exile_id;
create table exile(
    id              decimal primary key default nextval('seq_exile_id'),
    pers_id         decimal        references pers(id)          default null,
    pers_nick       varchar(1024)                               default null,

    savior_id        decimal        references pers(id)          default null,
    savior_nick      varchar(1024)                               default null,

    sender_id       decimal        references pers(id)          default null,
    sender_nick     varchar(1024)                               default null,
    room_id         decimal        references room(doc_id)      default null,
    room_head       varchar(1024)                               default null,
    roomtype_id     decimal         references roomtype(id)     default null,
    roomtype_alias  varchar(1024)   references roomtype(alias)  default null,
    reason          text default null,
    created         timestamp       without time zone not null
        default utcnow(),
    expired         timestamp without time zone not null
        default utcnow() + interval '1 week',
    isdeleted       boolean default false
);




create sequence seq_experbuy_id;
create table experbuy (
    id                  decimal primary key default nextval('seq_experbuy_id'),

    /**
        Покупатель, тот кто платит
    **/
    buyer_id            decimal         references pers(id)     not null,
    buyer_nick          varchar(1024)   default null   not null,

    /**
        Владелец, тот кто обладает товаром после покупки
    **/
    owner_id            decimal         references pers(id)     not null,
    owner_nick          varchar(1024)   default null   not null,

    /**
        Вещь которую приобрели --- опыт.
        Нужно знать, какой количество
    **/
    expired               numeric             default null,
    price               numeric(1000, 2)    default null,

    created             timestamp without time zone not null default utcnow(),
    isdeleted           boolean default false
);




create sequence seq_zprotbuy_id;
create table zprotbuy(
    id                  decimal
        primary key default nextval('seq_zprotbuy_id'),
    buyer_id            decimal
        references pers(id)                 default null,
    buyer_nick          varchar(1024)       default null,
    owner_id            decimal
        references pers(id)                 default null,
    owner_nick          varchar(1024)       default null,
    price               numeric(1000, 2)    default null,
    created timestamp without time zone not null default utcnow(),
    expired timestamp without time zone not null
            default utcnow() + interval '1 week',
    isdeleted           boolean default false
);





-------------------------------------------------------------------------------
-- СВЯЗКИ МНОГИЕ КО МНОГИМ
-------------------------------------------------------------------------------

/**
 *  Многие ко многим для прав и групп
**/

create sequence seq_perm2pgroup_id;
create table perm2pgroup (
    perm2pgroup_id      decimal primary key default nextval('seq_perm2pgroup_id'),
    perm_id decimal     references perm(id) not null,
    group_id decimal    references pgroup(id) not null,
    created             timestamp without time zone not null default utcnow(),
    isdeleted           boolean default false
);

/**
 *  Многие ко многим для пользователей и групп
**/
create sequence seq_pers2pgroup_id;
create table pers2pgroup (
    pers2pgroup_id      decimal primary key default nextval('seq_pers2pgroup_id'),
    pers_id decimal     references pers(id) not null,
    group_id decimal    references pgroup(id) not null,
    created         timestamp without time zone not null default utcnow(),
    isdeleted           boolean default false
);


/**
 *  Многие ко многим для комнат и тем
**/
create sequence seq_room2topic_id;
create table room2topic (
    id                  decimal primary key default     nextval('seq_room2topic_id'),
    topic_id            decimal references topic(id)    not null,
    room_id             decimal references room(doc_id) not null,
    isdeleted           boolean    default false,
    created             timestamp without time zone not null default utcnow(),
    constraint          room2topic_topic_id_room_id_many_key    unique (topic_id, room_id)
);


/**
 *  Многие ко многим для комнат и тем
**/
create sequence seq_community2topic_id;
create table community2topic (
    id                  decimal primary key default     nextval('seq_community2topic_id'),
    topic_id            decimal references topic(id)    not null,
    community_id             decimal references room(doc_id) not null,
    isdeleted           boolean    default false,
    created             timestamp without time zone not null default utcnow(),
    constraint          room2topic_topic_id_room_id_many_key    unique (topic_id, room_id)
);


