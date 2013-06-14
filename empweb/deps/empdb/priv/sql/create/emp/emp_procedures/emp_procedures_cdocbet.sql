\echo :FILE 'in'



/**
    @doc Обеспечивает совместное состояние ставки аукциона
**/
create or replace function cdocbet_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Аукцион и предмет аукциона
    **/
    if not (new.cdoclot_id is null) then
        new.cdoc_id =
            (select cdoc_id from cdoclot where cdoclot.doc_id = new.cdoclot_id);
        new.cdoc_head =
            (select cdoc_head from cdoclot where cdoclot.doc_id = new.cdoclot_id);
    end if;
    /**
        Владелец заявки
    **/
    if (new.owner_nick is null) then
        if not (new.owner_id is null) then
            new.owner_nick =
                (select pers.nick from pers where pers.id = new.owner_id);
        end if;
    end if;
    if (new.owner_id is null) then
        new.owner_id           =
            (select pers.id from pers where pers.nick = new.owner_nick);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1cdocbet_util_fields_on_insert on cdocbet ;
create trigger t1cdocbet_util_fields_on_insert before insert
on cdocbet for each row execute procedure cdocbet_util_fields_on_insert();


create or replace function cdocbet_util_fields_after_insert() returns "trigger" as $$
begin
    if not (new.cdoc_id is null) then
        update doc set
            cdocbet_id          = new.id,
            cdocbet_price       = new.price,
            cdocbet_owner_id    = new.owner_id,
            cdocbet_owner_nick  = new.owner_nick
        where
            doc.id = new.cdoc_id;
    end if;
    if not (new.cdoclot_id is null) then
        update cdoclot set
            betcur              = new.price
        where
            cdoclot.doc_id = new.cdoclot_id;
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1cdocbet_util_fields_after_insert on cdocbet ;
create trigger t1cdocbet_util_fields_after_insert after insert
on cdocbet for each row execute procedure cdocbet_util_fields_after_insert();



create or replace function cdocbet_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Аукцион и предмет аукциона
    **/
    if new.cdoclot_id != old.cdoclot_id then
        new.cdoc_id =
            (select cdoc_id from cdoclot where cdoclot.doc_id = new.cdoclot_id);
        new.cdoc_head =
            (select cdoc_head from cdoclot where cdoclot.doc_id = new.cdoclot_id);
    end if;
    /**
        Владелец заявки
    **/
    if new.owner_id != old.owner_id then
        new.owner_nick =
            (select pers.nick from pers where pers.id = new.owner_id);
        update doc set
            cdocbet_owner_nick  = new.owner_nick,
            cdocbet_owner_id    = new.owner_id
        where
            doc.id = new.cdoc_id;
    end if;
    /** ------------------------------------------ **/
    if new.owner_nick != old.owner_nick then
        new.owner_id =
            (select pers.id from pers where pers.nick = new.owner_nick);
        update doc set
            cdocbet_owner_nick  = new.owner_nick,
            cdocbet_owner_id    = new.owner_id
        where
            doc.id = new.cdoc_id;
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1cdocbet_util_fields_on_update on cdocbet ;
create trigger t1cdocbet_util_fields_on_update before update
on cdocbet for each row execute procedure cdocbet_util_fields_on_update();




\echo :FILE ok
