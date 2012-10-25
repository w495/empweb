

/**
    Стоймость недостатока опыта.
    Сколько стоит то, что не хватает для перехода на следующий уровень.
**/

--     alter table pers add column experlackprice real default null;
--     update pers set experlackprice = 0.5 * experlack;
--     insert into doctype(alias) values('roomlot');

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
alter table room add column roombet_price       decimal                                     default null;*/



-- alter table roomlot add column betcur               numeric(1000, 2)                            default 0;
alter table room    add column roombet_owner_nick   varchar(1024)       references pers(nick)   default null;

