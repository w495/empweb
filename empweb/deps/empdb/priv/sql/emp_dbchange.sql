

/*
    alter table pers        alter column money              type numeric(1000, 2);
    alter table pers        alter column exper              type numeric;
    alter table pers        alter column experlack          type numeric;
    alter table pers        alter column experlackprice     type numeric(1000, 2);
    alter table roomlot     alter column betmin             type numeric(1000, 2);
    alter table roomlot     alter column betmax             type numeric(1000, 2);
    alter table roombet     alter column price              type numeric(1000, 2);
    alter table thing       alter column price              type numeric(1000, 2);
    alter table thingbuy    alter column price              type numeric(1000, 2);
    alter table experbuy    alter column exper              type numeric;
    alter table experbuy    alter column price              type numeric(1000, 2);
    alter table pay         alter column price              type numeric(1000, 2);
    alter table room add column roomlot_id          decimal         references roomlot(doc_id)  default null;
    alter table room add column roomlot_betmin      numeric(1000, 2)                            default null;
    alter table room add column roomlot_betmax      numeric(1000, 2)                            default null;
    alter table room add column roomlot_dtstart     timestamp without time zone not null default utcnow();
    alter table room add column roomlot_dtstop      timestamp without time zone not null default utcnow() + interval '1 week';
    alter table room add column roombet_id          decimal        references roombet(id)       default null;
    alter table room add column roombet_owner_id    decimal        references pers(id)          default null;
    alter table room add column roombet_price       decimal                                     default null;
    alter table roomlot add column betcur               numeric(1000, 2)                            default 0;
    alter table room    add column roombet_owner_nick   varchar(1024)       references pers(nick)   default null;
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
*/


create table photo(
    doc_id              decimal unique references doc(id),
    ncomments           decimal default 0,
    file_id             decimal references pers(id)     default null,
    path                varchar(1024)   default null,
    /**
        Разрешение на перепост
    **/
    is_cover            bool default false
);



    alter table fileinfo add column md5         char(32)                        not null;
    alter table fileinfo add column filetype_id decimal references filetype(id) default null;
    alter table fileinfo add column token       varchar(1024)                   default null;
    
    alter table file add column ulfileinfo_id   decimal references fileinfo(id)    default null;
    alter table file add column dlfileinfo_id   decimal references fileinfo(id)    default null;
    alter table file add column fsfileinfo_id   decimal references fileinfo(id)    default null;



    insert into filetype (alias, mime, ext) values
        ('undefined',         'application/octet-stream',   'undefined'),
        ('image/gif *.gif',   'image/gif',                  'gif'),
        ('image/jpeg *.jpeg', 'image/jpeg',                 'jpeg'),
        ('image/jpeg *.jpg',  'image/jpeg',                 'jpg'),
        ('image/png *.png',   'image/png',                  'png');

        
