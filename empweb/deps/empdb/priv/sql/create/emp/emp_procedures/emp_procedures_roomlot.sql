\echo :FILE 'in'




--(2012.10.22 16:01:55:586728391)---------------------------------------------


/**
    @doc Обеспечивает совместное состояние лота аукциона
**/
create or replace function roomlot_util_fields_on_insert() returns "trigger" as $$
begin
    if not (new.room_id is null) then
        new.room_head =
            (select head from doc where doc.id = new.room_id);
    end if;
    new.betcur = new.betmin;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roomlot_util_fields_on_insert on roomlot ;
create trigger t1roomlot_util_fields_on_insert before insert
on roomlot for each row execute procedure roomlot_util_fields_on_insert();


create or replace function roomlot_util_fields_after_insert() returns "trigger" as $$
begin
    if not (new.room_id is null) then
        update room set
            roomlot_id      = new.doc_id,
            roomlot_betmin  = new.betmin,
            roomlot_betmax  = new.betmax,
            roomlot_dtstart = new.dtstart,
            roomlot_dtstop  = new.dtstop
        where
            room.doc_id = new.room_id;
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roomlot_util_fields_after_insert on roomlot ;
create trigger t1roomlot_util_fields_after_insert after insert
on roomlot for each row execute procedure roomlot_util_fields_after_insert();

create or replace function roomlot_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Плательщик покупки
    **/
    if (new.room_id != old.room_id) then
        new.room_head =
            (select head from doc where doc.id = new.room_id);
        if (not (new.room_id is null)) then
            update room set
                roomlot_id      = null,
                roomlot_betmin  = null,
                roomlot_betmax  = null,
                roomlot_dtstart = null,
                roomlot_dtstop  = null
            where
                room.doc_id = old.room_id;
            update room set
                roomlot_id      = new.doc_id,
                roomlot_betmin  = new.betmin,
                roomlot_betmax  = new.betmax,
                roomlot_dtstart = new.dtstart,
                roomlot_dtstop  = new.dtstop
            where
                room.doc_id = new.room_id;
        end if;
    end if;

    if (new.betmin != old.betmin) and (not (new.room_id is null)) then
        update room set
            roomlot_betmin  = new.betmin
        where
            room.doc_id = new.room_id;
    end if;

    if (new.betmax != old.betmax) and (not (new.room_id is null)) then
        update room set
            roomlot_betmax  = new.betmax
        where
            room.doc_id = new.room_id;
    end if;

    if (new.dtstart != old.dtstart) and (not (new.room_id is null)) then
        update room set
            roomlot_dtstart  = new.dtstart
        where
            room.doc_id = new.room_id;
    end if;

    if (new.dtstop != old.dtstop) and (not (new.room_id is null)) then
        update room set
            roomlot_dtstop  = new.dtstop
        where
            room.doc_id = new.room_id;
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roomlot_util_fields_on_update on roomlot ;
create trigger t1roomlot_util_fields_on_update before update
on roomlot for each row execute procedure roomlot_util_fields_on_update();



\echo :FILE ok
