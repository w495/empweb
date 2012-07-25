-- ALTER TABLE legal_info ADD COLUMN inn character varying(11);
-- ALTER TABLE legal_info ADD COLUMN kpp character varying(10);
-- ALTER TABLE legal_info DROP COLUMN company_name;
-- 
-- ---Datetime создания видео
-- ALTER TABLE acv_video ADD COLUMN created timestamp without time zone;
-- ALTER TABLE acv_video ALTER COLUMN created SET DEFAULT now();
-- UPDATE acv_video SET created = now();
-- ALTER TABLE acv_video ALTER COLUMN created SET NOT NULL;
--
-- /**
--  * Тип рекламной компании для видео.
-- **/
-- create sequence seq_acv_estate;
-- create table acv_estate (
--     id              int primary key default nextval('seq_acv_estate'),
--     name            varchar(32) unique,
--     alias           varchar(128) default null,
--     description     varchar(1024) default null,
--     perm_id         int references permission(id) default null
-- );
-- 
-- alter table acv_video add column estate_id  int references acv_estate(id) default null;
-- update acv_video set estate_id  = null;




-- /**
--  * Регион подсети Марьино-нет
-- **/
-- create sequence seq_snet_region_id;
-- create table snet_region (
--     id int primary key default nextval('seq_snet_region_id'),
--     name            varchar(128) unique,
--     alias           varchar(128) default null,
--     description     varchar(1024) default null
-- );
-- 
-- 
-- /**
--  * Непосредственно подсеть.
-- **/
-- create sequence seq_snet_id;
-- create table snet (
--     id                  int primary key default nextval('seq_snet_id'),
--     name                varchar(128)    default null,
--     alias               varchar(128)    default null,
--     description         varchar(1024)   default null,
--     ip                  inet            default null,
--     region_id           int references  snet_region(id);
-- );
-- 
-- 
-- 
-- create table acv_video2snet_region (
--     -- id int primary key default nextval('seq_acv_video_2_geo_region'),
--     acv_video_id int references acv_video(id),
--     snet_region_id int references snet_region(id)
-- );
-- 
-- 
-- ALTER TABLE acv_video ADD COLUMN snet_ufwich character varying(1024);
-- ALTER TABLE acv_video ALTER COLUMN snet_ufwich SET DEFAULT null;
-- UPDATE acv_video SET snet_ufwich = null;
-- 
-- ALTER TABLE acv_video ALTER COLUMN snet_ufwich SET DEFAULT null;


-- insert into acv_estate (name, alias, description, perm_id)
--     values
--         (
--             'identification',
--             'заставка tvzavr',
--             'заставка о том, кто ведет вещание',
--             (select id from permission where name='admin')
--         ),
--         (
--             'sprev',
--             'специальная пред-реклама',
--             'специальная реклама от заказчиков, идет перед обычной',
--             (select id from permission where name='admin')
--         ),
--         (
--             'spost',
--             'специальная пост-реклама',
--             'специальная реклама от заказчиков, идет после обычной',
--             (select id from permission where name='admin')
--         );
-- 
-- 
-- ALTER TABLE customer ALTER COLUMN   patronimic TYPE  varchar(1024);
-- ALTER TABLE customer  ALTER COLUMN   patronimic  DROP NOT NULL;
-- ALTER TABLE customer  ALTER COLUMN patronimic SET DEFAULT null;
-- 
-- 


-- ALTER TABLE acv_video ADD COLUMN event_show character varying(1024);
-- ALTER TABLE acv_video ALTER COLUMN event_show SET DEFAULT null;
-- UPDATE acv_video SET event_show = null;
-- 
-- ALTER TABLE acv_video ADD COLUMN event_fullshow character varying(1024);
-- ALTER TABLE acv_video ALTER COLUMN event_fullshow SET DEFAULT null;
-- UPDATE acv_video SET event_fullshow = null;
-- 
-- ALTER TABLE acv_video ADD COLUMN event_click character varying(1024);
-- ALTER TABLE acv_video ALTER COLUMN event_click SET DEFAULT null;
-- UPDATE acv_video SET event_click = null;клик на рекламный креатив
-- 
-- ALTER TABLE acv_video ADD COLUMN event_ufwich character varying(1024);
-- ALTER TABLE acv_video ALTER COLUMN event_ufwich SET DEFAULT null;
-- UPDATE acv_video SET event_ufwich = null;

-----------------------------------------------------------------------------
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-----------------------------------------------------------------------------

create sequence seq_acv_video_event_type;
create table acv_video_event_type (
    id          int primary key default nextval('seq_acv_video_event_type'),
    name        varchar(1024) unique,
    description varchar(1024)

);

create sequence seq_acv_video_action;
create table acv_video_action (
    id              int primary key default nextval('seq_acv_video_action'),
    name            varchar(1024) unique,
    description     varchar(1024)
);


create sequence seq_acv_video_event;
create table acv_video_event (
    id              int primary key default nextval('seq_acv_video_event'),
    type_id         int references acv_video_event_type(id) default null,
    action_id       int references acv_video_action(id)     default null,
    acv_video_id    int references acv_video(id)            default null,
    comment         varchar(1024) default null,
    url             varchar(1024) default null
);

-- ALTER TABLE acv_video_event ADD COLUMN comment character varying(1024);
-- ALTER TABLE acv_video_event ALTER COLUMN comment SET DEFAULT null;
-- UPDATE acv_video_event SET comment = null;

insert into acv_video_event_type (name, description)
    values
        ('show',        'начало показа рекламного креатива'),
        ('fullshow',    'конец показа рекламного креатива'),
        ('click',       'клик на рекламный креатив');


insert into acv_video_action (name, description)
    values('get',        'сделать http get на url. результат не используется');


insert into acv_estate (name, alias, description, perm_id)
    values (
        'empty',
        'пустая реклама',
        'место для рекламы, которая не будет показана',
        (select id from permission where name='admin')
    );