

/*
    alter table pers        alter column money
        type numeric(1000, 2);
    alter table pers        alter column exper
        type numeric;
    alter table pers        alter column experlack
        type numeric;
    alter table pers        alter column experlackprice
        type numeric(1000, 2);
    alter table roomlot     alter column betmin
        type numeric(1000, 2);
    alter table roomlot     alter column betmax
        type numeric(1000, 2);
    alter table roombet     alter column price
        type numeric(1000, 2);
    alter table thing       alter column price
        type numeric(1000, 2);
    alter table thingbuy    alter column price
        type numeric(1000, 2);
    alter table experbuy    alter column exper
        type numeric;
    alter table experbuy    alter column price
        type numeric(1000, 2);
    alter table pay         alter column price
        type numeric(1000, 2);
    alter table room add column roomlot_id
        decimal         references roomlot(doc_id)  default null;
    alter table room add column roomlot_betmin
        numeric(1000, 2)                            default null;
    alter table room add column roomlot_betmax
        numeric(1000, 2)                            default null;
    alter table room add column roomlot_dtstart
        timestamp without time zone not null default utcnow();
    alter table room add column roomlot_dtstop
        timestamp without time zone not null
            default utcnow() + interval '1 week';
    alter table room add column roombet_id
        decimal        references roombet(id)       default null;
    alter table room add column roombet_owner_id
        decimal        references pers(id)          default null;
    alter table room add column roombet_price
        decimal                                     default null;
    alter table roomlot add column betcur
        numeric(1000, 2)                            default 0;
    alter table room    add column roombet_owner_nick
        varchar(1024)       references pers(nick)   default null;
    insert into paytype(alias, isincome)
        values  ('roombet_out',     false),
                ('roombet_in',      true ),
                ('roomlot_in',      true ),
                ('thing_out',       false),
                ('exper_out',       false);
    alter table room         add column treas numeric(1000, 2) default 1;
    insert into paytype(alias, isincome)
        values  ('treas_out',     false),
                ('treas_in',      true );
    insert into paytype(alias, isincome)
        values  ('room_out',     false),
                ('room_in',      true );
    insert into treasoptype (alias, isincome)
        values  ('transfer_in',     true),
                ('transfer_out',    false);
    alter table room         alter column  ulimit SET DEFAULT 5000;
*/


-- 2012-11-14_13-21-56-591819368 --------------------------------------------

/*
    insert into doctype(alias) values  ('album');
    insert into doctype(alias) values  ('photo');
*/

-- 2012-11-15_17-49-02-997410202 --------------------------------------------

/*
    insert into communitytype(alias) values ('elite');
    alter table pers add column live_room_pos   numeric default null;
    create table photo(
        doc_id              decimal unique references doc(id),
        ncomments           decimal default 0,
        file_id             decimal references file(id)     default null,
        filepath           varchar(1024)   default null,
        is_cover            bool default false
    );
    alter table fileinfo add column md5
        char(32)                        not null;
    alter table fileinfo add column filetype_id
        decimal references filetype(id) default null;
    alter table fileinfo add column token
        varchar(1024)                   default null;
    alter table file add column ulfileinfo_id
        decimal references fileinfo(id) default null;
    alter table file add column dlfileinfo_id
        decimal references fileinfo(id) default null;
    alter table file add column fsfileinfo_id
        decimal references fileinfo(id) default null;
    insert into filetype (alias, mime, ext) values
        ('undefined',         'application/octet-stream',   'undefined'),
        ('image/gif *.gif',   'image/gif',                  'gif'),
        ('image/jpeg *.jpeg', 'image/jpeg',                 'jpeg'),
        ('image/jpeg *.jpg',  'image/jpeg',                 'jpg'),
        ('image/png *.png',   'image/png',                  'png');
*/

-- 2012.11.20 13:55:45:041000861  --------------------------------------------

/*
    alter table fileinfo add tokenlong       decimal    default null;
    alter table fileinfo add tokenstring     char(128)  default null;
    alter table fileinfo add md5long         decimal    default null;
    alter table fileinfo add md5string       char(32)   default null;
    alter table file add tokenlong           decimal        default null;
    alter table file add tokenstring         char(128)      default null;
    alter table file add owner_id
        decimal    references pers(id) default null;
    alter table fileinfo add owner_id
        decimal    references pers(id) default null;
    alter table file     add owner_nick
        varchar(1024)   references pers(nick) default null;
    alter table fileinfo add owner_nick
        varchar(1024)   references pers(nick) default null;
    alter table fileinfo add column doc_id
        decimal references doc(id) default null;
    alter table file add column doc_id
        decimal references doc(id) default null;
    create sequence seq_fileinfotype_id;
    create table fileinfotype(
        id          decimal primary key default nextval('seq_fileinfotype_id'),
        name_ti     decimal unique      default nextval('seq_any_ti'),
        alias       varchar(1024)       unique,
        created     timestamp without time zone not null default utcnow(),
        isdeleted   bool default false
    );
    insert into fileinfotype(alias) values
        ('upload'),
        ('download'),
        ('filesystem');
    alter table fileinfo add column fileinfotype_id
        decimal references fileinfotype(id)         default null;
    alter table fileinfo add column fileinfotype_alias
        varchar(1024)  default null;
    alter table fileinfo add column filetype_alias
        varchar(1024)  default null;
    alter table fileinfo add column file_id
        decimal references file(id) default null;
*/

-- alter table photo rename column path to filepath;



-- 2012.11.20 19:22:23:726564884 --------------------------------------------

/*
    create sequence seq_geo_id;
    create table geo(
        id          decimal primary key default nextval('seq_geo_id'),
        alias       varchar(1024)   default null,
        name_ti     decimal unique      default nextval('seq_any_ti'),
        descr_ti    decimal unique      default nextval('seq_any_ti'),
        parent_id    decimal references geo(id) default null,
        nchildtargets   decimal default 0,
        nnodetargets    decimal default 0,
        nchildren       decimal default 0,
        nnodes          decimal default 0,
        created         timestamp without time zone not null default utcnow(),
        isdeleted       bool default false
    );
    alter table pers add column geo_id decimal references geo(id) default null;
    alter table pers add column interest varchar(1024) default null;
    alter table pers add column pregion_id numeric default null;
    insert into geo(alias) values
        ('ru'),
        ('kz'),
        ('uk'),
        ('by'),
        ('ua'),
        ('jp'),
        ('cn');
    insert into geo(alias, parent_id) values
        ('moscow',      (select id from geo where alias = 'ru')),
        ('rudnyj',      (select id from geo where alias = 'ru')),
        ('kazan',       (select id from geo where alias = 'ru')),
        ('anadyr',      (select id from geo where alias = 'ru')),
        ('astana',      (select id from geo where alias = 'kz')),
        ('alma-ata',    (select id from geo where alias = 'kz')),
        ('rudnyj',      (select id from geo where alias = 'kz')),
        ('misk',        (select id from geo where alias = 'by')),
        ('brest',       (select id from geo where alias = 'by')),
        ('kiev',        (select id from geo where alias = 'ua')),
        ('odessa',      (select id from geo where alias = 'ua'));

    alter table friend add column pers_nick     varchar(1024) default null;
    alter table friend  add column friend_nick  varchar(1024) default null;
*/


-- 2012.11.23 00:27:16:733548113 --------------------------------------------

/*

    alter table pers add column live_community_id
        decimal references community(doc_id) default null;
    alter table pers add column own_community_id
        decimal references community(doc_id) default null;
    alter table pers add column
        live_community_head varchar(1024) default null;
    alter table pers add column
        own_community_head  varchar(1024) default null;
    alter table pers add column live_community_approved
        boolean  default null;
    alter table community add
        column ncands decimal default 0;
    alter table community add
        column nmembs decimal default 0;
    alter table photo add column path varchar(1024) default null;
    alter table room add column roombet_owner_nick
        varchar(1024)  default null;
    -- alter table room alter column roomlot_dtstart    drop  not null;
    alter table room alter column roomlot_dtstop        drop not null;

*/

-- 2012.11.29 12:28:58:757638537 --------------------------------------------

/*
    create sequence seq_noticetype_id;
    create table noticetype(
        id
            decimal primary key default nextval('seq_noticetype_id'),
        name_ti
            decimal unique  default nextval('seq_any_ti'),
        alias
            varchar(1024)   unique,
        created
            timestamp       without time zone not null default utcnow(),
        isdeleted
            bool            default false
    );
    create table notice(
        doc_id
            decimal         unique references doc(id),
        noticetype_id
            decimal         references noticetype(id)       default null,
        noticetype_alias
            varchar(1024)   references noticetype(alias)    default null,
        datetime
            timestamp       without time zone             default utcnow()
    );
    insert into doctype(alias) values ('notice');
    alter table notice add column pers_id   decimal
        references pers(id) default null;
    alter table notice add column pers_nick varchar(1024)
        references pers(nick) default null;
    alter table rptrans alter column transtype_id drop not null;
    alter table rptrans alter column transtype_alias drop not null;
    alter table roomtreas alter column treastype_id drop not null;
    alter table roomtreas alter column treastype_alias drop not null;
*/

-- 2012.11.30 19:13:49:175377606 --------------------------------------------

/*
    create sequence seq_communityhisttype_id;
    create table communityhisttype(
        id          decimal primary key
            default nextval('seq_communityhisttype_id'),
        name_ti
            decimal unique  default nextval('seq_any_ti'),
        alias
            varchar(1024)   unique,
        created
            timestamp       without time zone not null default utcnow(),
        isdeleted   bool            default false
    );
    insert into communityhisttype(alias) values
        ('pers_cand'),
        ('pers_memb'),
        ('pers_out'),
        ('pers_exile');
    create sequence seq_communityhist_id;
    create table communityhist(
        id                  decimal
            primary key default nextval('seq_communitycand_id'),
        pers_id             decimal
            references pers(id) not null,
        pers_nick           varchar(1024)
            references pers(nick) not null,
        community_id                decimal
            references roomlot(doc_id)  default null,
        communityhisttype_id        decimal
            references communityhisttype(id)    default null,
        communityhisttype_alias     varchar(1024)
            references communityhisttype(alias) default null,
        created
            timestamp without time zone not null default utcnow(),
        isdeleted           bool default false
    );
*/

-- 2012.12.07 12:31:59:027268735 --------------------------------------------

/*
    create sequence seq_friendtype_id;
    create table friendtype (
        id          decimal primary key default nextval('seq_friendtype_id'),

        name_ti     decimal unique default nextval('seq_any_ti'),
        alias       varchar(1024)   unique,
        created     timestamp without time zone not null default utcnow(),
        isdeleted   bool    default false
    );
    alter table friend add column friendtype_id
        decimal         references friendtype(id)       default null;
    alter table friend add column friendtype_alias
        varchar(1024)   references friendtype(alias)    default null;
    insert into friendtype(alias) values
        ('foe'),
        ('friend');
*/

-- 2012.12.10 15:07:26:274198859 --------------------------------------------

/*
    create sequence seq_thingwish_id;
    create table thingwish (
        id
            decimal primary key default nextval('seq_thingwish_id'),
        buyer_id            decimal         references pers(id)     not null,
        buyer_nick          varchar(1024)   references pers(nick)   not null,
        owner_id            decimal         references pers(id)     not null,
        owner_nick          varchar(1024)   references pers(nick)   not null,
        thing_id            decimal         references thing(id)    not null,
        thing_alias         varchar(1024)   references thing(alias) not null,
        price               numeric(1000, 2) default null,
        counter
            timestamp without time zone not null default utcnow(),
        created
            timestamp without time zone not null default utcnow(),
        isdeleted           bool default false
    );
    create sequence seq_vote_id;
    create table vote(
        id
            decimal primary key default nextval('seq_vote_id'),
        doc_id
            decimal         references doc(id),
        pers_id
            decimal         references pers(id),
        pers_nick
            varchar(1024)   references pers(nick),
        rating
            numeric         default null,
        created
            timestamp without time zone not null default utcnow(),
        isdeleted           bool default false,
        constraint
            room2topic_doc_id_pers_id_many_key    unique (doc_id, pers_id)
    );
    alter  table post  drop column nvotes;
    alter  table blog  drop column nvotes;
    alter  table doc  drop column vcounter;
    alter table doc add column nvotes numeric default 0;
    alter table doc add column nviews numeric default 0;

*/

-- 2012.12.11 16:58:01:647478891 --------------------------------------------

/*
    alter table pers add column isempl      boolean         default null;
    alter table pers add column isempl      boolean         default null;
    alter table doc           drop constraint  doc_owner_nick_fkey;
    alter table pay           drop constraint  pay_pers_nick_fkey;
    alter table vote          drop constraint  vote_pers_nick_fkey;
    alter table file          drop constraint  file_owner_nick_fkey;
    alter table fileinfo      drop constraint  fileinfo_owner_nick_fkey;
    alter table communityhist drop constraint  communityhist_pers_nick_fkey;
    alter table message       drop constraint  message_reader_nick_fkey;
    alter table roombet       drop constraint  roombet_owner_nick_fkey;
    alter table room          drop constraint  room_roombet_owner_nick_fkey;
    alter table thingbuy      drop constraint  thingbuy_buyer_nick_fkey;
    alter table thingbuy      drop constraint  thingbuy_owner_nick_fkey;
    alter table experbuy      drop constraint  experbuy_buyer_nick_fkey;
    alter table experbuy      drop constraint  experbuy_owner_nick_fkey;
    alter table rptrans       drop constraint  rptrans_pers_nick_fkey;
    alter table roomtreas     drop constraint  roomtreas_pers_nick_fkey;
    alter table thingwish     drop constraint  thingwish_buyer_nick_fkey;
    alter table thingwish     drop constraint  thingwish_owner_nick_fkey;
    insert into paytype(alias, isincome)
        values  ('change_nick',     false);
*/

-- 2012.12.16 19:31:02:859818643 --------------------------------------------

/*
    create table claim(
        doc_id              decimal unique references doc(id),
        pers_id             decimal         references pers(id),
        pers_nick           varchar(1024)   default null
    );
    alter table claim add column judge_id numeric references pers(id) default null;
    alter table claim add column judge_nick varchar(1024) default null;
*/

-- 2012.12.16 20:34:28:513375684 --------------------------------------------

/*
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
        isdeleted         bool default false
    );
    alter table doc add column orig_id
        decimal         references doc(id)      default null;
    alter table doc add column orig_owner_id
        decimal         references pers(id)     default null;
    alter table doc add column orig_owner_nick
        varchar(1024)                           default null;
    alter table doc add column isrepost
        bool default false;
    alter table doc add column isrepostable
        bool default true;
*/


-- 2012.12.18 16:59:05:953080627 --------------------------------------------

/*
    alter table thingwish drop column buyer_id;
    alter table thingwish drop column buyer_nick;
    alter table thingwish drop column counter;
*/

-- 2012.12.19 16:59:05:953080627 --------------------------------------------

/*
    alter table topic add column  nroomtargets  decimal default 0;
    alter table topic add column  ncommunitytargets  decimal default 0;
    create sequence seq_community2topic_id;
    create table community2topic (
        id
            decimal primary key default     nextval('seq_community2topic_id'),
        topic_id
            decimal references topic(id)    not null,
        community_id
            decimal references community(doc_id) not null,
        isdeleted
            bool    default false,
        created
            timestamp without time zone not null default utcnow(),
        constraint
            community2topic_topic_id_community_id_many_key    unique (topic_id, community_id)
    );
    alter table room add column authority_id
        decimal         references authority(id)        default null;
    alter table room add column authority_alias
        varchar(1024)   references authority(alias)     default null;
    alter table room add column exper
        numeric         default 0;
    alter table room add column experlack
        numeric         default null;
    alter table room add column experlackprice
        numeric(1000, 2)         default null;
    create sequence seq_roomexperbuy_id;
    create table roomexperbuy (
        id
            decimal primary key default nextval('seq_roomexperbuy_id'),
        room_id
            decimal         references room(doc_id)      default null,
        room_head
            varchar(1024)                            default null,
        exper
            numeric             default null,
        price
            numeric(1000, 2)    default null,
        created
            timestamp without time zone not null default utcnow(),
        isdeleted
            bool default false
    );
    insert into treastype (alias, isincome)
        values  ('exper_out',       false);
*/

-- 2012.12.21 13:16:08:401186853 ---------------------------------------------

/*
    create sequence seq_cptrans_id;
    create table cptrans (
        id
            decimal primary key default nextval('seq_cptrans_id'),
        pers_id
            decimal         references pers(id)     not null,
        pers_nick
            varchar(1024)   default null   not null,
        community_id
            decimal         references community(doc_id)       not null,
        transtype_id
            decimal         references transtype(id)      default null,
        transtype_alias
            varchar(1024)   references transtype(alias)   default null,
        price
            numeric(1000, 2)    default null,
        created
            timestamp without time zone not null default utcnow(),
        isdeleted
            bool default false
    );
    create sequence seq_communitytreas_id;
    create table communitytreas (
        id
            decimal primary key default nextval('seq_communitytreas_id'),
        pers_id
            decimal         references pers(id)     not null,
        pers_nick
            varchar(1024)   default null   not null,
        community_id
            decimal         references community(doc_id)       not null,
        treastype_id
            decimal         references treastype(id)      default null,
        treastype_alias
            varchar(1024)   references treastype(alias)   default null,
        isincome
            bool            default null,
        price
            numeric(1000, 2)    default null,
        info
            varchar(1024)   default null,
        created
            timestamp without time zone not null default utcnow(),
        isdeleted
            bool default false
    );
    alter table community add column treas
        decimal default 0;
    alter table community add column fee
        decimal default 0;
    alter table pers add column live_roomtype_id
        decimal references roomtype(id)     default null;
    alter table pers add column live_roomtype_alias
        varchar(1024) references roomtype(alias)     default null;
    alter table pers add column isprisoner
        boolean default false;
    insert into paytype(alias, isincome)
        values  ('community_out',     false),
                ('community_in',      true );
    alter table pers add  column citizen_room_id
        decimal references room(doc_id) default null;
    alter table pers add column citizen_room_head
        varchar(1024) default null;
    alter table pers add column citizen_room_fromdatetime
        timestamp without time zone default null;
*/

-- 2012.12.24 15:55:05:901896410 ---------------------------------------------

/*
    alter table community add  column cands_gte_authority_id
        decimal         references authority(id) default null;

    alter table community add  column cands_gte_authority_alias
        varchar(1024)   references authority(alias) default null;
*/


-- 2012.12.25 13:05:06:298990484 ---------------------------------------------

/*
    alter table pers add column live_community_rejectreason
        text default null;
*/

-- 2012.12.25 17:22:06:355620587 ---------------------------------------------

/*
    alter table community add column read_gte_authority_id
        decimal references authority(id) default null;
    alter table community add column read_gte_authority_alias
        varchar(1024) references authority(alias) default null;
    alter table community add column cands_gte_authority_id
        decimal references authority(id) default null;
    alter table community add column cands_gte_authority_alias
        varchar(1024) references authority(alias) default null;
    alter table community add column read_gte_authority_level
        numeric default null;
    alter table community add column cands_gte_authority_level
        numeric default null;
    alter table pers add column authority_level
        numeric default null;
    create sequence seq_actiontype_id;
    create table actiontype(
        id          decimal primary key default nextval('seq_actiontype_id'),
        name_ti     decimal unique  default nextval('seq_any_ti'),
        alias       varchar(1024)   unique,
        ispaid      boolean         default false,
        price       decimal         default 0,
        created     timestamp without time zone not null default utcnow(),
        isdeleted   boolean default false
    );
    insert into doctype(alias)
        values  ('claim');
    alter table room add column back_file_id
        decimal references file(id)     default null;
    alter table room add column back_path
        varchar(1024)                   default null;
    alter table room add column wall_file_id
        decimal references file(id)     default null;
    alter table room add column wall_path
        varchar(1024)                   default null;
    alter table room add column flag_file_id
        decimal references file(id)     default null;
    alter table room add column flag_path
        varchar(1024)                   default null;
    alter table room add column arms_file_id
        decimal references file(id)     default null;
    alter table room add column arms_path
        varchar(1024)                   default null;
    alter table community add column back_file_id
        decimal references file(id)     default null;
    alter table community add column back_path
        varchar(1024)                   default null;
    alter table community add column wall_file_id
        decimal references file(id)     default null;
    alter table community add column wall_path
        varchar(1024)                   default null;
    alter table community add column flag_file_id
        decimal references file(id)     default null;
    alter table community add column flag_path
        varchar(1024)                   default null;
    alter table community add column arms_file_id
        decimal references file(id)     default null;
    alter table community add column arms_path
        varchar(1024)                   default null;
    create sequence seq_community2topic_id;
    create table community2topic (
        id
            decimal primary key default
                nextval('seq_community2topic_id'),
        topic_id
            decimal references topic(id)    not null,
        community_id
            decimal references community(doc_id) not null,
        isdeleted
            bool    default false,
        created
            timestamp without time zone not null default utcnow(),
        constraint
            community2topic_topic_id_community_id_many_key
                unique (topic_id, community_id)
    );
*/

-- 2012.12.28 16:55:45:155698946 ---------------------------------------------

/*
    create table wall(
        doc_id              decimal unique references doc(id),
        file_id             decimal references file(id)     default null
    );
    create table back(
        doc_id              decimal unique references doc(id),
        file_id             decimal references file(id)     default null
    );
    create table flag(
        doc_id              decimal unique references doc(id),
        file_id             decimal references file(id)     default null
    );
    create table arms(
        doc_id              decimal unique references doc(id),
        file_id             decimal references file(id)     default null
    );
    create sequence seq_actiontype_id;
    create table actiontype(
        id          decimal primary key default nextval('seq_actiontype_id'),
        name_ti     decimal unique      default nextval('seq_any_ti'),
        alias       varchar(1024)       unique,
        ispaid      boolean             default false,
        istoall     boolean             default false,
        price       numeric(1000, 2)    default 0,
        created     timestamp without time zone not null default utcnow(),
        isdeleted   boolean default false
    );



    insert into actiontype(alias, ispaid, price, istoall)
        values
            ('advertisement',           false, null, false),
            ('apologize',               false, null, false),
            ('cheerup',                 true,  0.01, false),
            ('compliment',              false, null, false),
            ('congratulate',            false, null, false),
            ('congratulate_all',        false, 0.90, true),
            ('declarelove',             false, null, false),
            ('embrace_all',             true,  0.01, false),
            ('embracepassionately',     false, null, false),
            ('enjoy',                   false, null, false),
            ('fireworks',               false, null, false),
            ('hello',                   false, null, false),
            ('hello_all',               false, null, true ),
            ('goodbye',                 false, null, false),
            ('goodbye_all',             true,  0.01, true ),
            ('goodnight',               false, null, false),
            ('handson',                 true,  0.01, false),
            ('hug',                     true,  0.01, false),
            ('hug_all',                 true,  0.01, true ),
            ('hugfriendly',             false, null, false),
            ('kick',                    true,  0.01, false),
            ('kick_all',                true,  1.00, true ),
            ('kiss',                    false, null, false),
            ('kiss_all',                true,  0.50, true ),
            ('kisscheek',               false, null, false),
            ('kisspassionately',        false, null, false),
            ('kisstenderly',            false, null, false),
            ('lisp',                    true,  0.01, false),
            ('ogle',                    false, null, false),
            ('peck',                    false, null, false),
            ('pullpants',               true,  0.03, false),
            ('put',                     false, null, false),
            ('sendflower',              true,  0.01, false),
            ('sendkiss',                false, null, false),
            ('sendall',                 false, null, false),
            ('shakehands',              false, null, false),
            ('shakehands_all',          false, 0.40, true ),
            ('shutup',                  true,  0.03, false),
            ('shutup_all',              true,  0.30, true ),
            ('slap',                    true,  0.01, false),
            ('takehandle',              true,  0.02, false),
            ('takeumbrage',             false, null, false),
            ('unfastenbra',             true,  0.03, false),
            ('welcome',                 true,  null, false),
            ('welcome_all',             true,  0.01, true );
    create sequence seq_action_id;
    create table action(
        id                  decimal primary key
            default nextval('seq_action_id'),
        actiontype_id       decimal references actiontype(id)
            default null,
        actiontype_alias    varchar(1024) references actiontype(alias)
            default null,
        pers_id             decimal references pers(id)     default null,
        pers_nick           varchar(1024)                   default null,
        owner_id            decimal references pers(id)     default null,
        owner_nick          varchar(1024)                   default null,
        ispaid      boolean         default false,
        price       numeric(1000, 2)    default 0,
        created     timestamp       without time zone not null
            default utcnow(),
        expired     timestamp without time zone not null
            default utcnow() + interval '1 week',
        isdeleted   boolean         default false
    );
    alter table doc add column isnoticeable
         boolean default false;
    insert into paytype(alias, isincome)
        values  ('action_out',     false);
*/


-- 2013.01.11 14:43:20:318454342 ---------------------------------------------

/*
    create sequence seq_ostatus_id;
    create table ostatus(
        id          decimal primary key default nextval('seq_ostatus_id'),
        name_ti     decimal unique default nextval('seq_any_ti'),
        alias       varchar(1024)   unique,
        created     timestamp without time zone not null default utcnow(),
        isdeleted   boolean default false
    );
    alter table pers add column  ostatus_id
        decimal         references ostatus(id)    default null;
    alter table pers add column ostatus_alias
        varchar(1024)   references ostatus(alias) default null;
    insert into ostatus(alias)
        values ('citizen'),('police'),('officer');
    update pers set ostatus_alias = 'citizen';
    update pers set ostatus_id = (select id from ostatus where alias = 'citizen');
    update pers set ismale = true;
    alter table pers alter column ismale set default true;
    create sequence seq_cstatus_id;
    create table cstatus(
        id          decimal primary key default nextval('seq_cstatus_id'),
        name_ti     decimal unique default nextval('seq_any_ti'),
        alias       varchar(1024)   unique,
        created     timestamp without time zone not null default utcnow(),
        isdeleted   boolean default false
    );
    alter table pers add column  cstatus_id
        decimal         references cstatus(id)    default null;
    alter table pers add column cstatus_alias
        varchar(1024)   references cstatus(alias) default null;
    insert into cstatus(alias)
        values ('citizen'),('police'),('officer');
    update pers set cstatus_alias = 'citizen';
    update pers set cstatus_id = (select id from cstatus where alias = 'citizen');

*/

-- 2013.01.16 13:50:06:579642727 ---------------------------------------------

/*
    create sequence seq_claimtype_id;
    create table claimtype(
        id          decimal primary key default nextval('seq_claimtype_id'),
        name_ti     decimal unique      default nextval('seq_any_ti'),
        alias       varchar(1024)   unique,
        created     timestamp without time zone not null default utcnow(),
        isdeleted   boolean default false
    );
    insert into claimtype(alias)
        values  ('open'),
                ('progress'),
                ('fixed'),
                ('wontfix'),
                ('closed');
    alter table claim add column claimtype_id
        decimal references claimtype(id)     default null;
    alter table claim add column claimtype_alias
        varchar(1024) references claimtype(alias)     default null;
    alter table claim add column live_room_approved
        boolean  default true;
*/

-- 2013.01.28 13:38:10:998016462 ---------------------------------------------

/*
    alter table community add column isclosed
        boolean default false;
    create sequence seq_roomlisttype_id;
    create table roomlisttype(
        id          decimal primary key default nextval('seq_roomlisttype_id'),
        name_ti     decimal unique      default nextval('seq_any_ti'),
        alias       varchar(1024)   unique,
        created     timestamp without time zone not null default utcnow(),
        isdeleted   boolean default false
    );
    insert into roomlisttype (alias) values ('black');
    create sequence seq_roomlist_id;
    drop table roomlist;
    create table roomlist(
        id          decimal primary key default nextval('seq_roomlist_id'),
        pers_id     decimal        references pers(id)       default null,
        pers_nick   varchar(1024)                            default null,

        room_id     decimal         references room(doc_id)  default null,
        room_head   varchar(1024)                            default null,

        roomlisttype_id        decimal
            references roomlisttype(id)        default null,
        roomlisttype_alias     varchar(1024)
            references roomlisttype(alias)     default null,

        reason      text default null,
        created     timestamp without time zone not null default utcnow(),
        isdeleted   boolean default false,
        constraint  roomlist_pers_id_room_id_roomlisttype_id_many_key
            unique (pers_id, room_id, roomlisttype_id)
    );
    insert into treastype (isincome,    alias) values (true, 'fee_in');
    insert into paytype (isincome,    alias) values (false, 'roomlist_delete');
    ALTER TABLE roomlist DROP CONSTRAINT
        roomlist_pers_id_room_id_roomlisttype_id_many_key;
    ALTER TABLE roomlist ALTER  pers_id SET NOT NULL;
    ALTER TABLE roomlist ALTER  room_id SET NOT NULL;
    ALTER TABLE roomlist ALTER  roomlisttype_id SET NOT NULL;
    alter table actiontype add column isforme
        boolean default false;
    alter table actiontype add column isfake
        boolean default false;
    insert into actiontype(isincome,    alias) values (true, 'fee_in');
    insert into actiontype(alias, ispaid, price, istoall, isforme, isfake)
        values
            ('add_roomblacklist',           true, 3.0, false, false, true),
            ('delete_roomblacklist',        true, 2.0, false, true,  true);
*/


-- 2013.01.30 18:50:23:043426130 ---------------------------------------------

/*
    create sequence seq_exile_id;
    create table exile(
        id              decimal
            primary key default nextval('seq_exile_id'),
        pers_id         decimal
            references pers(id)             default null,
        pers_nick       varchar(1024)       default null,
        savior_id        decimal
            references pers(id)             default null,
        savior_nick      varchar(1024)      default null,

        sender_id       decimal
            references pers(id)             default null,
        sender_nick     varchar(1024)       default null,
        room_id         decimal
            references room(doc_id)         default null,
        room_head       varchar(1024)       default null,
        roomtype_id     decimal
            references roomtype(id)         default null,
        roomtype_alias  varchar(1024)
            references roomtype(alias)      default null,
        reason          text                default null,
        created         timestamp           without time zone not null
            default utcnow(),
        expired         timestamp without time zone not null
            default utcnow() + interval '1 week',
        isdeleted       boolean default false
    );
    insert into paytype(alias, isincome) values  ('exile_delete',    false);
    insert into paytype(alias, isincome) values  ('roomlist_delete', false);
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
    insert into paytype(alias, isincome) values  ('zprotbuy',    false);
*/

-- 2013.01.31 18:50:23:043426130 ---------------------------------------------

/*
    create sequence seq_invistype_id;
    create table invistype(
        id          decimal
            primary key default nextval('seq_invistype_id'),
        name_ti     decimal
            unique default nextval('seq_any_ti'),
        alias       varchar(1024)   unique,
        price       numeric(1000, 2) default 0,
        created     timestamp
            without time zone not null default utcnow(),
        isdeleted   boolean    default false
    );
    insert into invistype(alias, price) values  ('visible',         0.00);
    insert into invistype(alias, price) values  ('partly_visible',  1.00);
    insert into invistype(alias, price) values  ('invisible',       2.00);
    update pers set invistype_alias = 'visible';
    update pers set invistype_id =
        (select id from invistype where alias = 'visible');
    alter table pers add column invistype_id
        decimal         references invistype(id)  default null;
    alter table pers add column invistype_alias
        varchar(1024)   references invistype(alias)  default null;
    create sequence seq_invisbuy_id;
    create table invisbuy(
        id                  decimal
            primary key default nextval('seq_invisbuy_id'),
        buyer_id            decimal
            references pers(id)                 default null,
        buyer_nick          varchar(1024)       default null,
        owner_id            decimal
            references pers(id)                 default null,
        owner_nick          varchar(1024)       default null,
        invistype_id        decimal
            references invistype(id)  default null,
        invistype_alias     varchar(1024)
            references invistype(alias)  default null,
        price               numeric(1000, 2)    default null,
        created timestamp without time zone not null default utcnow(),
        isdeleted           boolean default false
    );
    insert into paytype(alias, isincome) values  ('invisbuy',    false);
*/

-- 2013.02.01 13:54:32:927137381 ---------------------------------------------

/*
    alter table pers add column invistype_level decimal default 0;

    alter table invistype add column level decimal default 0;

    alter table invisbuy add column invistype_level decimal default 0;

    update invistype set level = 0 where alias = 'visible';

    update invistype set level = 50 where alias = 'partly_visible';

    update invistype set level = 100 where alias = 'invisible';


    */

-- 2013.02.04 16:43:50:192810755 ---------------------------------------------

/*
    alter table pers drop column birthday;
    alter table pers add column
        birthday timestamp without time zone default null;
    alter table event add column
        pers_nick varchar(1024) default null;
*/

-- 2013.02.05 18:12:53:880912613 ---------------------------------------------

/*
    alter table post add column
        pic_file_id decimal references file(id) default null;

    alter table doc add column
        isrepostcont boolean default false;
*/

-- 2013.02.05 19:15:58:283727927 ---------------------------------------------

/*
    create sequence seq_service_id;
    create table service(
        id          decimal
            primary key default nextval('seq_service_id'),
        name_ti     decimal
            unique default nextval('seq_any_ti'),
        alias       varchar(1024)   unique,
        price       numeric(1000, 2) default 0,
        expired     timestamp without time zone not null
            default utcnow() + interval '1 week',
        created     timestamp
            without time zone not null default utcnow(),
        isonce      boolean    default true,
        isdeleted   boolean    default false
    );
    insert into service(alias, price, isonce)
        values ('create_room_price', 1.0,       true);
    insert into service(alias, price, isonce)
        values ('create_community_price',  1.0, true);
    insert into service(alias, price, isonce)
        values ('delete_exile_price_other',2.0, true);
    insert into service(alias, price, isonce)
        values ('delete_exile_price_self', 3.0, true);
    insert into service(alias, price, isonce)
        values ('delete_roomlist_price', 2.0,   true);
    insert into service(alias, price, isonce)
        values ('create_experbuy_coef', 0.5,    false);
    insert into service(alias, price, isonce)
        values ('create_zprotbuy_coef', 0.5,    false);
    insert into eventtype (alias) values ('new_community_cand');
    insert into eventtype (alias) values ('new_community_memb');
    insert into eventtype (alias) values ('new_community_out');
    insert into eventtype (alias) values ('new_community_exile');
    insert into eventtype (alias) values ('new_community_away');
    insert into communityhisttype (alias) values ('pers_away');
    insert into eventtype (alias) values ('create_mail');
*/

-- 2013.02.21 17:25:58:980890996 ---------------------------------------------

/*
    alter table eventtype add column isnews boolean  default false;
    insert into communityhisttype (alias) values ('pers_away');
    insert into eventtype (alias) values ('new_community_cand');
    insert into eventtype (alias) values ('new_community_memb');
    insert into eventtype (alias) values ('new_community_out');
    insert into eventtype (alias) values ('new_community_exile');
    insert into eventtype (alias) values ('new_community_away');
    insert into eventtype (alias) values ('create_mail');
    insert into eventtype (alias) values ('birthday_today');
    insert into eventtype (alias) values ('add_friend');
    insert into eventtype (alias) values ('delete_friend');
    insert into eventtype (alias) values ('create_message');
    insert into eventtype (alias, isnews) values ('create_post', true);
    insert into eventtype (alias, isnews) values ('repost_post', true);
    insert into eventtype (alias, isnews) values ('create_photo', true);
    insert into eventtype (alias, isnews) values ('repost_photo', true);
    insert into eventtype (alias, isnews) values ('repost_post_my', false);
    insert into eventtype (alias, isnews) values ('repost_photo_my', false);
    create sequence seq_event_id;
    create table event(
        id                  decimal primary key default     nextval('seq_event_id'),
        head                text,
        body                text default null,
        owner_id            decimal references pers(id) default null,
        owner_nick          varchar(1024)               default null,
        pers_id             decimal references pers(id) default null,
        pers_nick           varchar(1024)               default null,
        friendtype_id decimal           references friendtype(id)       default null,
        friendtype_alias varchar(1024)  references friendtype(alias)    default null,
        eventtype_id        decimal         references eventtype(id)    default null,
        eventtype_alias     varchar(1024)   references eventtype(alias) default null,
        eventobj_id        decimal         references eventobj(id)    default null,
        eventobj_alias     varchar(1024)   references eventobj(alias) default null,
        eventact_id        decimal         references eventact(id)    default null,
        eventact_alias     varchar(1024)   references eventact(alias) default null,
        eventspc_id        decimal         references eventspc(id)    default null,
        eventspc_alias     varchar(1024)   references eventspc(alias) default null,
        target_id       decimal                                     default null,
        doc_id          decimal         references doc(id)          default null,
        doc_head        varchar(1024)                               default null,
        doc_owner_id    decimal         references pers(id)         default null,
        doc_owner_nick  varchar(1024)                               default null,
        doc_parent_id   decimal         references doc(id)          default null,
        oktype_id       decimal         references oktype(id)       default null,
        oktype_alias    varchar(1024)   references oktype(alias)    default null,
        doctype_id      decimal         references doctype(id)      default null,
        doctype_alias   varchar(1024)   references doctype(alias)   default null,
        contype_id      decimal         references contype(id)      default null,
        contype_alias   varchar(1024)   references contype(alias)   default null,
        orig_id         decimal         references doc(id)          default null,
        orig_owner_id   decimal         references pers(id)         default null,
        orig_owner_nick varchar(1024)                               default null,
        isnews              boolean  default false,
        created             timestamp without time zone not null default utcnow(),
        isdeleted           boolean default false
    );
    create sequence seq_eventobj_id;
    create table eventobj(
        id          decimal primary key default nextval('seq_eventobj_id'),
        name_ti     decimal unique  default nextval('seq_any_ti'),
        alias       varchar(1024)   unique,
        isnews      boolean  default false,
        created     timestamp       without time zone not null default utcnow(),
        isdeleted   boolean            default false
    );
    create sequence seq_eventact_id;
    create table eventact(
        id          decimal primary key default nextval('seq_eventact_id'),
        name_ti     decimal unique  default nextval('seq_any_ti'),
        alias       varchar(1024)   unique,
        isnews      boolean  default false,
        created     timestamp       without time zone not null default utcnow(),
        isdeleted   boolean            default false
    );
    create sequence seq_eventspc_id;
    create table eventspc(
        id          decimal primary key default nextval('seq_eventspc_id'),
        name_ti     decimal unique  default nextval('seq_any_ti'),
        alias       varchar(1024)   unique,
        isnews      boolean  default false,
        created     timestamp       without time zone not null default utcnow(),
        isdeleted   boolean            default false
    );
    insert into eventtype (alias, isnews) values ('create_exile', true);
    insert into eventtype (alias, isnews) values ('delete_exile_save', true);
    insert into eventtype (alias, isnews) values ('delete_exile_remove_expired', true);
    alter table event add column
        target_id decimal default null;
    alter table event add column
        eventobj_id        decimal         references eventobj(id)    default null;
    alter table event add column
        eventobj_alias     varchar(1024)   references eventobj(alias) default null;

    alter table event add column
        eventact_id        decimal         references eventact(id)    default null;
    alter table event add column
        eventact_alias     varchar(1024)   references eventact(alias) default null;

    alter table event add column
        eventspc_id        decimal         references eventspc(id)    default null;
    alter table event add column
        eventspc_alias     varchar(1024)   references eventspc(alias) default null;
    insert into eventact (alias) values ('create');
    insert into eventact (alias) values ('delete');
    insert into eventact (alias) values ('update');
    insert into eventact (alias) values ('repost');
    insert into eventobj (alias) values ('photo');
    insert into eventobj (alias) values ('post');
    insert into eventobj (alias) values ('blog');
    insert into eventobj (alias) values ('exile');
    insert into eventobj (alias) values ('pers');
    insert into eventobj (alias) values ('message');
    insert into eventobj (alias) values ('friend');
*/


-- 2013.02.22 11:57:31:552364540 ---------------------------------------------

/*

    insert into eventobj (alias) values ('roomlot');
    insert into eventobj (alias) values ('roombet');
    insert into eventtype (alias, isnews) values ('delete_roomlot_expired', false);
    insert into eventtype (alias, isnews) values ('delete_roombet_beatrate', false);
    insert into eventtype (alias, isnews) values ('create_roombet_win', false);
    insert into eventtype (alias, isnews) values ('delete_roomlot_win', false);
    alter table pers add column
        istimeover          boolean default false;
    alter table pers add column
        isostatusable       boolean         default true;
    alter table doc add column
        head_ti     decimal unique default nextval('seq_any_ti');
    alter table doc add column
        body_ti     decimal unique default nextval('seq_any_ti');
*/


-- 2013.02.27 10:08:19:307711396 ---------------------------------------------

/*
    alter table pers drop column perspichead_id ;
    alter table pers drop column perspicbody_id ;
    drop table perspicbody ;
    drop table perspichead ;
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
    alter table pers add column
        perspichead_id     decimal references perspichead(doc_id);
    alter table pers add column
        perspicbody_id     decimal references perspicbody(doc_id);
*/

-- 2013.03.01 15:10:30:261555319 ---------------------------------------------

/*
    alter table thing add column
        thingtype_alias    varchar(1024)    default null;
    alter table thing add column
        price           numeric(1000, 2)   default null;
    alter table thing add column
        rent            numeric(1000, 2)   default null;
    alter table thing add column
        file_id         decimal references file(id) default null;
    alter table thingtype add column
        file_id         decimal references file(id) default null;
    alter table thingbuy add column
        rent            numeric(1000, 2)   default null;
    alter table thingbuy add column
        expired             timestamp without time zone          default null;
    alter table thingwish add column
        rent                numeric(1000, 2) default null;
*/

-- 2013.03.05 16:02:01:632280169 ---------------------------------------------

/*
    alter table pers add column
        costume_thingbuy_id decimal references thingbuy(id) default null;
    alter table thingbuy add column
        thingtype_id       decimal references thingtype(id) default null;
    alter table thingbuy add column
        thingtype_alias    varchar(1024)                    default null;
    alter table thingbuy add column
        thingtype_alias    varchar(1024)                    default null;

    alter table claim add column
        room_id decimal references doc(id) default null;
    alter table claim add column
        room_head varchar(1024)            default null;
    alter table claim add column
        ss_owner_citizen_room_id decimal references doc(id) default null;
    alter table claim add column
        ss_owner_citizen_room_head varchar(1024)            default null;
    alter table claim add column
        ss_owner_live_room_id decimal references doc(id) default null;
    alter table claim add column
        ss_owner_live_room_head varchar(1024)            default null;
    alter table claim add column
        ss_pers_citizen_room_id decimal references doc(id) default null;
    alter table claim add column
        ss_pers_citizen_room_head varchar(1024)            default null;
    alter table claim add column
        ss_pers_live_room_id decimal references doc(id) default null;
    alter table claim add column
        ss_pers_live_room_head varchar(1024)            default null;
*/


-- 2013.03.05 17:15:30:160369728 ---------------------------------------------

/*
    insert into eventtype (alias) values ('new_community_not_enough_money');
    insert into eventtype (alias) values ('new_community_not_enough_level');
    insert into eventtype (alias) values ('new_community_not_enough_level_not_enough_money');
    insert into communityhisttype (alias) values ('pers_not_enough_money');
    insert into communityhisttype (alias) values ('pers_not_enough_level');
    insert into communityhisttype (alias) values ('pers_not_enough_level_not_enough_money');
*/

/*

    alter table pers add column
        show_money_acctype_id     decimal         references acctype(id)    default null;
    alter table pers add column
        show_money_acctype_alias  varchar(1024)   references acctype(alias) default null;
    alter table pers add column
        get_message_acctype_id     decimal         references acctype(id)    default null;
    alter table pers add column
        get_message_acctype_alias  varchar(1024)   references acctype(alias) default null;
    alter table pers add column
        get_thingbuy_acctype_id     decimal         references acctype(id)    default null;
    alter table pers add column
        get_thingbuy_acctype_alias  varchar(1024)   references acctype(alias) default null;

*/

-- 2013.03.13 12:31:52:495564760 ---------------------------------------------

/*
    alter table wall  add column
        isdefault boolean default false;
    alter table back  add column
        isdefault boolean default false;
    alter table flag  add column
        isdefault boolean default false;
    alter table arms  add column
        isdefault boolean default false;
*/

-- 2013.03.26 14:38:43:486101457  ---------------------------------------------
/*
    alter table filetype add column
        mimesuptype varchar(1024) default null;
    alter table filetype add column
        mimesubtype varchar(1024) default null;
    update filetype set mimesuptype  =  'image' where id in (1,2,3,4);
    update filetype set mimesubtype  =  'gif' where id  = 1;
    update filetype set mimesubtype  =  'jpeg' where id  = 2;
    update filetype set mimesubtype  =  'jpeg' where id  = 3;
    update filetype set mimesubtype  =  'png' where id  = 4;
    alter table fileinfo add column
        image_width decimal default null;
    alter table fileinfo add column
        image_height decimal default null;
    alter table fileinfo add column
        filetype_mime varchar(1024) default null;
    alter table fileinfo add column
        filetype_mimesuptype varchar(1024) default null;
    alter table fileinfo add column
        filetype_mimesubtype varchar(1024) default null;
    alter table fileinfo add column
        filetype_ext varchar(1024) default null;
    alter table file drop column ulfileinfo_id;
    alter table file drop column dlfileinfo_id;
    alter table file drop column fsfileinfo_id;
    update fileinfo set filetype_mimesuptype = 'image' where filetype_alias like '%image%';
    update fileinfo set filetype_mimesubtype = 'gif' where filetype_alias like '%gif%';
    update fileinfo set filetype_mimesubtype = 'gif' where filetype_alias like '%png%';
    update fileinfo set filetype_mimesubtype = 'png' where filetype_alias like '%png%';
    update fileinfo set filetype_mimesubtype = 'jpeg' where filetype_alias like '%jpeg%';
*/

-- 2013.05.07 14:54:00:909981100 ---------------------------------------------

/*
    alter table file add column alias varchar(1024) default null;
    alter table bitrate add created timestamp without time zone not null default utcnow(),
    alter table perspicbody add column ismale boolean default null;
    alter table perspichead add column ismale boolean default null;
    insert into actiontype(alias, ispaid, price, istoall, isforme, isfake)
        values),
            ('fly',        false, null, false, true,  true);
    alter table pers add column perspicphoto_id  decimal references file(id)   default null;
*/

-- 2013.05.13 19:02:23:330929088 ---------------------------------------------


/*
    insert into service(alias, price, isonce)
        values ('change_nick_price', 1.0,       true);
    insert into service(alias, price, isonce)
        values ('change_perspichead_price', 1.0,       true);
    insert into service(alias, price, isonce)
        values ('change_perspicbody_price', 1.0,       true);
    insert into paytype(alias, isincome)
        values  ('change_perspichead',     false),
                ('change_perspicbody',     false);
    alter table claim add column  isoffer  boolean default null;
*/


-- 2013.05.16 12:40:40:891241891 ---------------------------------------------

/*
    alter table thingbuy add column
        room_id decimal references doc(id)   default null;
    alter table thingbuy add column
        room_head varchar(1024)  default null;

    alter table thingbuy add column
        costs  numeric(1000, 2)  default null;
    insert into paytype(alias, isincome)
        values  ('thingforme_out',     false);
    insert into paytype(alias, isincome)
        values  ('thingforhim_out',     false);
    insert into paytype(alias, isincome)
        values  ('thingforit_out',     false);
    alter TABLE thingbuy alter column buyer_id  drop not null;
    alter TABLE thingbuy alter column buyer_nick  drop not null;
    alter TABLE thingbuy alter column owner_nick  drop not null;
    alter TABLE thingbuy alter column owner_id  drop not null;
    alter TABLE thingbuy alter column thing_id  drop not null;
    alter TABLE thingbuy alter column thing_alias  drop not null;
    alter TABLE thingbuy alter column thing_alias  drop not null;
    alter table thing add column aliasnum numeric default null;
    alter table doc add column
        lang_id  decimal references chatlang(id) default null;
    alter table doc add column
        lang_alias varchar(1024) references chatlang(alias) default null;
    alter table thingwish add column
    thingtype_id       decimal references thingtype(id) default null;
    alter table thingwish add column
    thingtype_alias    varchar(1024)                    default null;
*/


-- 2013.06.14 16:37:49:884106689 ---------------------------------------------


/*
    create table communitylot(
        doc_id          decimal unique references doc(id),
        community_id         decimal references community(doc_id)        default null,
        community_head       varchar(1024)  default null,
        dtstart         timestamp without time zone not null default utcnow(),
        dtstop          timestamp without time zone not null default utcnow() + interval '1 week',
        betmin          numeric(1000, 2) default 0,
        betcur          numeric(1000, 2) default 0,
        betmax          numeric(1000, 2) default 100
    );
    alter table communitylot add column community_id          decimal         references community(doc_id)   default null;
    alter table communitylot add column community_head       varchar(1024)  default null;
    create sequence seq_communitybet_id;
    create table communitybet(
        id              decimal primary key default nextval('seq_communitybet_id'),
        communitylot_id      decimal         references communitylot(doc_id)      default null,
        community_id         decimal             default null,
        community_head       varchar(1024)  default null,
        owner_id        decimal             references pers(id)     not null,
        owner_nick      varchar(1024)       default null   not null,
        price           numeric(1000, 2)    default 0,
        created         timestamp without time zone not null    default utcnow(),
        isdeleted       boolean    default false
    );
    alter table community add column
        communitylot_id          decimal         references communitylot(doc_id)  default null;
    alter table community add column
        communitylot_betmin      numeric(1000, 2)                            default null;
    alter table community add column
        communitylot_betmax      numeric(1000, 2)                            default null;
    alter table community add column
        ommunitylot_dtstart     timestamp without time zone not null default utcnow();
    alter table community add column
        communitylot_dtstop      timestamp without time zone not null default utcnow() + interval '1 week';
    alter table community add column
        communitybet_id          decimal        references communitybet(id)       default null;
    alter table community add column
        communitybet_owner_id    decimal        references pers(id)          default null;
    alter table community add column
        communitybet_owner_nick  varchar(1024)                               default null;
    alter table community add column
        communitybet_price       decimal                                     default null;
    insert into paytype(alias, isincome)
        values  ('communitybet_out',     false),
                ('communitybet_in',      true ),
                ('communitylot_in',      true );
    insert into eventobj (alias) values ('communitylot');
    insert into eventobj (alias) values ('communitybet');
    insert into eventtype (alias, isnews) values ('delete_communitylot_expired', false);
    insert into eventtype (alias, isnews) values ('delete_communitybet_beatrate', false);
    insert into eventtype (alias, isnews) values ('create_communitybet_win', false);
    insert into eventtype (alias, isnews) values ('delete_communitylot_win', false);

*/

-- 2013.06.19 12:58:52:922344838 ---------------------------------------------

/*
    create table cdoclot(
        doc_id          decimal unique references doc(id),
        cdoc_id         decimal references doc(id)        default null,
        cdoc_head       varchar(1024)  default null,
        dtstart         timestamp without time zone not null default utcnow(),
        dtstop          timestamp without time zone not null default utcnow() + interval '1 week',
        betmin          numeric(1000, 2) default 0,
        betcur          numeric(1000, 2) default 0,
        betmax          numeric(1000, 2) default 100
    );
    create sequence seq_cdocbet_id;
    create table cdocbet(
        id              decimal primary key default nextval('seq_cdocbet_id'),
        cdoclot_id      decimal         references cdoclot(doc_id)      default null,
        cdoc_id         decimal             default null,
        cdoc_head       varchar(1024)  default null,
        owner_id        decimal             references pers(id)     not null,
        owner_nick      varchar(1024)       default null   not null,
        price           numeric(1000, 2)    default 0,
        created         timestamp without time zone not null    default utcnow(),
        isdeleted       boolean    default false
    );
    alter table cdocbet add column
        cdoclot_id      decimal         references cdoclot(doc_id)      default null;
    alter table doc add column
        cdoclot_id          decimal         references cdoclot(doc_id)  default null;
    alter table doc add column
        cdoclot_betmin      numeric(1000, 2)                            default null;
    alter table doc add column
        cdoclot_betmax      numeric(1000, 2)                            default null;
    alter table doc add column
        cdoclot_dtstart     timestamp without time zone not null default utcnow();
    alter table doc add column
        cdoclot_dtstop      timestamp without time zone not null default utcnow() + interval '1 week';
    alter table doc add column
        cdocbet_id          decimal        references cdocbet(id)       default null;
    alter table doc add column
        cdocbet_owner_id    decimal        references pers(id)          default null;
    alter table doc add column
        cdocbet_owner_nick  varchar(1024)                               default null;
    alter table doc add column
        cdocbet_price       decimal                                     default null;
    insert into paytype(alias, isincome)
        values  ('cdocbet_out',     false),
                ('cdocbet_in',      true ),
                ('cdoclot_in',      true );
    insert into eventobj (alias) values ('cdoclot');
    insert into eventobj (alias) values ('cdocbet');
    insert into eventtype (alias, isnews) values ('delete_cdoclot_expired', false);
    insert into eventtype (alias, isnews) values ('delete_cdocbet_beatrate', false);
    insert into eventtype (alias, isnews) values ('create_cdocbet_win', false);
    insert into eventtype (alias, isnews) values ('delete_cdoclot_win', false);
*/

-- 2013.06.19 12:58:52:922344838 ---------------------------------------------

/*
    update pers set nick = 'a' || CAST (id as varchar(1024)) where id
        in ( select id from (select id, cl from (select id, char_length(nick)
            as cl , nick   from pers order by cl desc) as foo where foo.cl > 9)
                as bar);
    alter table pers  alter column nick type varchar(10);
    insert into doctype(alias)
        values  ('cdoclot');
    alter table cdoclot add column
        cdoctype_id          decimal         references doctype(id)    default null;
    alter table cdoclot add column
        cdoctype_alias       varchar(1024)   references doctype(alias) default null;
    alter table cdocbet add column
        cdoctype_id          decimal         references doctype(id)    default null;
    alter table cdocbet add column
        cdoctype_alias       varchar(1024)   references doctype(alias) default null;
*/


/*
    insert into paytype(alias, isincome) values  ('create_adult_only_album',    false);
    insert into service(alias, price, isonce)
        values ('create_adult_only_album_price',  20.0, true);
    alter table pers add column phonestr varchar(1024)  default null;
    alter table thingbuy add column
        community_id decimal references doc(id)   default null;
    alter table thingbuy add column
        community_head varchar(1024)  default null;
    create sequence seq_experwish_id;
    create table experwish (
        id                  decimal primary key default nextval('seq_experwish_id'),
        owner_id            decimal         references pers(id)     not null,
        owner_nick          varchar(1024)   default null   not null,
        exper               numeric             default null,
        price               numeric(1000, 2)    default null,
        created             timestamp without time zone not null default utcnow(),
        isdeleted           boolean default false
    );
    alter table pers add column experwish numeric  default 0;
    create sequence seq_moneywish_id;
    create table moneywish (
        id                  decimal primary key default nextval('seq_moneywish_id'),
        owner_id            decimal         references pers(id)     not null,
        owner_nick          varchar(1024)   default null   not null,
        money               numeric             default null,
        price               numeric(1000, 2)    default null,
        created             timestamp without time zone not null default utcnow(),
        isdeleted           boolean default false
    );
    alter table pers add column moneywish numeric(1000, 2)    default 0;
    alter table thingbuy add column isforwish boolean default false;
    alter table experbuy add column isforwish boolean default false;
*/

-- 2013.06.24 14:54:07:753472621 ---------------------------------------------


/*
    alter table communitytype add column authority_id
        decimal     references authority(id)     default null;

    alter table communitytype add column authority_alias
        varchar(1024)     references authority(alias)     default null;

    update community set  communitytype_alias  = 'common'
        where  communitytype_alias  is null;

    update
        communitytype
    set
        authority_id =
            (select id from authority where alias = 'inhabitant' )
    where
        alias = 'common';
    update
        communitytype
    set
        authority_alias = 'inhabitant'
    where
        alias = 'common';

    update
        communitytype
    set
        authority_id =
            (select id from authority where alias = 'citizen' )
    where
        alias = 'secret';
    update
        communitytype
    set
        authority_alias = 'citizen'
    where
        alias = 'secret';

    update
        communitytype
    set
        authority_id =
            (select id from authority where alias = 'elder' )
    where
        alias = 'elite';
    update
        communitytype
    set
        authority_alias = 'elder'
    where
        alias = 'elite';
*/

/*
    create sequence seq_pptrans_id;
    create table pptrans (
        id                 decimal primary key default nextval('seq_pptrans_id'),
        pers_id            decimal         references pers(id)          not null,
        pers_nick          varchar(1024)   default null   not null,
        receiver_id        decimal         references pers(id)          not null,
        transtype_id       decimal         references transtype(id)     default null,
        transtype_alias    varchar(1024)   references transtype(alias)  default null,
        price              numeric(1000, 2)    default null,
        created            timestamp without time zone not null default utcnow(),
        isdeleted          boolean default false
    );
    insert into paytype (alias,         isincome)
            values      ('pers_out',    false),
                        ('pers_in',     true );

   alter table photo drop column is_cover;
   alter table photo  add column iscover   boolean default false;
    alter table event add column
        doc_parent_head         varchar(1024)  default null;
    update event set doc_parent_head =
        (select doc.head from doc where doc.id = doc_parent_id);


    alter table perspichead add column
        alias         varchar(1024)  default null;


    alter table perspicbody add column
        alias         varchar(1024)  default null;

*/

    alter table fileinfo add column
        aspect_width  numeric default null;


    alter table fileinfo add column
        aspect_height  numeric default null;


    CREATE OR REPLACE FUNCTION gcd( a numeric,  b numeric)
    RETURNS numeric
    IMMUTABLE
    STRICT
    LANGUAGE SQL
    AS $$
    WITH RECURSIVE t(a,b) AS (
        VALUES (abs($1)::numeric, abs($2)::numeric)
    UNION ALL
        SELECT b, mod(a,b) FROM t
        WHERE b > 0
    )
    SELECT a FROM t WHERE b = 0
    $$;



    update fileinfo set aspect_width = image_width / (select gcd(image_width, image_height));

    update fileinfo set aspect_height = image_height / (select gcd(image_width, image_height));



    select fileinfo.image_width from fileinfo where fileinfo.file_id = file_id where fileinfo.image_width is not null;

    select fileinfo.image_height from fileinfo where fileinfo.file_id = file_id where fileinfo.image_height is not null;

    update fileinfo set aspect_width = (select fileinfo.image_width from fileinfo where fileinfo.file_id = file_id and fileinfo.image_width is not null limit 1) / (select gcd((select fileinfo.image_width from fileinfo where fileinfo.file_id = file_id and fileinfo.image_width is not null limit 1), (select fileinfo.image_height from fileinfo where fileinfo.file_id = file_id and fileinfo.image_height is not null limit 1))) where image_width is null;

    update fileinfo set aspect_height = (select fileinfo.image_height from fileinfo where fileinfo.file_id = file_id and fileinfo.image_height is not null limit 1) / (select gcd((select fileinfo.image_width from fileinfo where fileinfo.file_id = file_id and fileinfo.image_width is not null limit 1), (select fileinfo.image_height from fileinfo where fileinfo.file_id = file_id and fileinfo.image_height is not null limit 1))) where image_width is null;

