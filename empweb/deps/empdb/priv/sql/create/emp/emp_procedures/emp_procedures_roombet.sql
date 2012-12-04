\echo :FILE 'in'



/**
    @doc Обеспечивает совместное состояние ставки аукциона
**/
create or replace function roombet_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Аукцион и предмет аукциона
    **/
    if not (new.roomlot_id is null) then
        new.room_id =
            (select room_id from roomlot where roomlot.doc_id = new.roomlot_id);
        new.room_head =
            (select room_head from roomlot where roomlot.doc_id = new.roomlot_id);
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

drop trigger if exists t1roombet_util_fields_on_insert on roombet ;
create trigger t1roombet_util_fields_on_insert before insert
on roombet for each row execute procedure roombet_util_fields_on_insert();


create or replace function roombet_util_fields_after_insert() returns "trigger" as $$
begin
    if not (new.room_id is null) then
        update room set
            roombet_id          = new.id,
            roombet_price       = new.price,
            roombet_owner_id    = new.owner_id,
            roombet_owner_nick  = new.owner_nick
        where
            room.doc_id = new.room_id;
    end if;
    if not (new.roomlot_id is null) then
        update roomlot set
            betcur              = new.price
        where
            roomlot.doc_id = new.roomlot_id;
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roombet_util_fields_after_insert on roombet ;
create trigger t1roombet_util_fields_after_insert after insert
on roombet for each row execute procedure roombet_util_fields_after_insert();



create or replace function roombet_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Аукцион и предмет аукциона
    **/
    if new.roomlot_id != old.roomlot_id then
        new.room_id =
            (select room_id from roomlot where roomlot.doc_id = new.roomlot_id);
        new.room_head =
            (select room_head from roomlot where roomlot.doc_id = new.roomlot_id);
    end if;
    /**
        Владелец заявки
    **/
    if new.owner_id != old.owner_id then
        new.owner_nick =
            (select pers.nick from pers where pers.id = new.owner_id);
        update room set
            roombet_owner_nick  = new.owner_nick,
            roombet_owner_id    = new.owner_id
        where
            room.doc_id = new.room_id;
    end if;
    /** ------------------------------------------ **/
    if new.owner_nick != old.owner_nick then
        new.owner_id =
            (select pers.id from pers where pers.nick = new.owner_nick);
        update room set
            roombet_owner_nick  = new.owner_nick,
            roombet_owner_id    = new.owner_id
        where
            room.doc_id = new.room_id;
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roombet_util_fields_on_update on roombet ;
create trigger t1roombet_util_fields_on_update before update
on roombet for each row execute procedure roombet_util_fields_on_update();




\echo :FILE ok
