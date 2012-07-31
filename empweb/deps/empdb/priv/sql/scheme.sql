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
    value_id    text default null,
    deleted     bool default false
);

create sequence seq_emotion_id;
create table emotion(
    id          int primary key default nextval('seq_emotion_id'),
    name_cid    int references content(id),
    deleted     bool default false
);


create sequence seq_status_id;
create table status(
    id          int primary key default nextval('seq_status_id'),
    name_cid    int references content(id),
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
    description     varchar(1024),
    phash           char(32) not null,

    email       varchar(1024) not null unique,
    phone       varchar(1024),
    fname       varchar(1024),
    sname       varchar(1024),
    city        varchar(1024),

    married_id          int references user_(id)   default null,
    mother_id           int references user_(id)   default null,
    father_id           int references user_(id)   default null,

    emotion_id          int references emotion(id)   default null,
    

    money               real,
    status_id           int references status(id)   default null,
    country_id          int default null,

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
    id      int primary key default nextval('seq_perm_entity_type'),
    name    varchar(1024) unique
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
    friend_id   int references user_(id)    default null
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
