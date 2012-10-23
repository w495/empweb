

/**
    Стоймость недостатока опыта.
    Сколько стоит то, что не хватает для перехода на следующий уровень.
**/

    alter table pers add column experlackprice real default null;

    update pers set experlackprice = 0.5 * experlack;

    insert into doctype(alias) values('roomlot');
