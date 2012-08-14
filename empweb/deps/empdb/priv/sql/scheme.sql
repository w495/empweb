/***********************************************************************
 *
 * \file Описание схемы базы данных
 *
***********************************************************************/


-------------------------------------------------------------------------------
-- ЛЮДИ 
-------------------------------------------------------------------------------

create sequence seq_lang_id;
create table lang(
    id int primary key default nextval('seq_lang_id'),
    name            varchar(1024),
    alias           varchar(1024),
    description     varchar(1024),
    deleted         bool default false
);  

create sequence seq_content_id;
create table content(
    id int primary key default nextval('seq_content_id'),
    cid         int default null,
    lang_id     int references lang(id)   default null,
    text        text default null,
    deleted     bool default false,
    constraint content_cid_lang_id_many_key unique (cid, lang_id)
);

create sequence seq_emotion_id;
create table emotion(
    id          int primary key default nextval('seq_emotion_id'),
    name_cid    int default  null,-- references content(cid),
    deleted     bool default false
);

create sequence seq_authority_id;
create table authority(
    id          int primary key default nextval('seq_authority_id'),
    name_cid    int default  null,-- references content(cid),
    level       int default 0,
    deleted     bool default false
);


create sequence seq_status_id;
create table status(
    id          int primary key default nextval('seq_status_id'),
    name_cid    int default  null,-- references content(cid),
    deleted     bool default false
);

create sequence seq_userpic_head_id;
create table userpic_head(
    id          int primary key default nextval('seq_userpic_head_id'),
    file        varchar(1024)   default  null,
    deleted     bool default false
);

create sequence seq_userpic_body_id;
create table userpic_body(
    id          int primary key default nextval('seq_userpic_body_id'),
    file        varchar(1024)   default  null,
    deleted     bool default false
);


/**
 * Рекламодатель
**/
create sequence seq_user_id;
create table user_(
    id int primary key default nextval('seq_user_id'),
    nick            varchar(1024) not null unique,
    name            varchar(1024),
    phash           char(32) not null,
    email           varchar(1024) not null unique,
    phone           varchar(1024),
    fname           varchar(1024),
    sname           varchar(1024),
    birthday        timestamp without time zone NOT NULL DEFAULT now(),
    male            bool default true,
    city            varchar(1024),
    married_status      bool default false,
    married_id          int references user_(id)   default null,
    description         varchar(1024),
    money               real,
    status_id           int references status(id)   default null,
    authority_id        int references authority(id)   default null,
    country_id          int default null,
    emotion_id          int references emotion(id)     default null,
    mother_id           int references user_(id)   default null,
    father_id           int references user_(id)   default null,
    community_id        int default null,
    employment          varchar(1024),
    hobby               varchar(1024),
    allow_auction_offer bool default false,
    userpic_body_id     int references authority(id)   default null,
    userpic_head_id     int references authority(id)   default null,
    created timestamp without time zone NOT NULL DEFAULT now(),
    deleted             bool default false
);  


/**
 * Группа пользователей
**/
create sequence seq_user_group_id;
create table user_group (
    id int primary key default nextval('seq_user_group_id'),
    name varchar(1024),
    description varchar(1024),
    issystem bool default false,
    deleted bool default false
);

/**
 * Типы прав
**/
create sequence seq_perm_type;
create table perm_type (
    id      int primary key default nextval('seq_perm_type'),
    name    varchar(1024) unique
);

/**
 * Типы cущностей прав
**/
create sequence seq_perm_entity_type;
create table perm_entity_type (
    id          int primary key default nextval('seq_perm_entity_type'),
    name        varchar(1024) unique
);


/**
 * Права
**/
create sequence seq_perm;
create table perm (
    id              int primary key default nextval('seq_perm'),
    perm_type_id    int references perm_type(id),
    entity_type_id  int references perm_entity_type(id),
    entity_id       int,
    name            varchar(1024),
    description     varchar(1024),
    type            int
);

create sequence seq_friend_id;
create table friend(
    id          int primary key default     nextval('seq_friend_id'),
    user_id     int references user_(id)    default null,
    friend_id   int references user_(id)    default null,
    constraint friend_user_id_friend_id_many_key unique     (user_id, friend_id)
);


-------------------------------------------------------------------------------
-- Документы
-------------------------------------------------------------------------------

-- Тип контента (блога и галереи). Приватный, дружеский, открытый.
create sequence seq_content_access_type_id;
create table content_access_type(
    id          int primary key default     nextval('seq_content_access_type_id'),
    alias       varchar(1024) unique,
    deleted     bool default false
);

-- Типы контента (обычный, эротический)
create sequence seq_content_type_id;
create table content_type(
    id          int primary key default     nextval('seq_content_type_id'),
    alias       varchar(1024) unique,
    deleted     bool default false
);


-- Тип документа - блог, коммент к блогу, галерея, фото, коммент к фото, attach description
create sequence seq_doc_type_id;
create table doc_type(
    id          int primary key default     nextval('seq_doc_type_id'),
    alias       varchar(1024) unique,
    deleted     bool default false
);


create sequence seq_doc_id;
create table doc(
    id                  int primary key default     nextval('seq_doc_id'),
    title               varchar(1024),
    content             varchar(1024) default null,
    doc_type_id         int references doc_type(id)     default null,
    content_type_id     int references content_type(id) default null,
    owner_id            int references user_(id)        default null,
    parent_id           int references doc(id)          default null,
    created timestamp without time zone NOT NULL DEFAULT now(),
    deleted             bool default false
);

create table blog(
    doc_id              int unique references doc(id),
    default_content_access_type_id  int references content_access_type(id)     default null,
    default_content_comment_type_id int references content_access_type(id)     default null,
    created timestamp without time zone NOT NULL DEFAULT now()
);


create sequence seq_blog_post_id;
create table blog_post(
    doc_id              int unique references doc(id),
    default_content_access_type_id  int references content_access_type(id)     default null,
    default_content_comment_type_id int references content_access_type(id)     default null,
    notice              bool default false,
    created timestamp without time zone NOT NULL DEFAULT now()
);



-------------------------------------------------------------------------------
-- СВЯЗКИ МНОГИЕ КО МНОГИМ
-------------------------------------------------------------------------------

/**
 * Многие ко многим для прав и групп
**/
create table perm2group (
    perm_id int references perm(id) not null,
    group_id int references user_group(id) not null
);

/**
 * Многие ко многим для пользователей и групп
**/
create table user2group (
    user_id int references user_(id) not null,
    group_id int references user_group(id) not null
);

