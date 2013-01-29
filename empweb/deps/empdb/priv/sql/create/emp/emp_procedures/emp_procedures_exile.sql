\echo :FILE 'in'

create or replace function exile_util_fields_on_insert() returns "trigger" as $$
begin
    if (new.pers_nick is null) then
        if not (new.pers_id is null) then
            new.pers_nick =
                (select pers.nick from pers where pers.id = new.pers_id);
        else
            new.pers_nick        = null;
        end if;
    end if;
    if (new.pers_id is null) then
        new.pers_id           =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;
    if (new.sender_nick is null) then
        if not (new.sender_id is null) then
            new.sender_nick =
                (select pers.nick from pers where pers.id = new.sender_id);
        else
            new.sender_nick        = null;
        end if;
    end if;
    if (new.sender_id is null) then
        new.sender_id           =
            (select pers.id from pers where pers.nick = new.sender_nick);
    end if;
    if (new.savior_nick is null) then
        if not (new.savior_id is null) then
            new.savior_nick =
                (select pers.nick from pers where pers.id = new.savior_id);
        else
            new.savior_nick        = null;
        end if;
    end if;
    if (new.savior_id is null) then
        new.savior_id           =
            (select pers.id from pers where pers.nick = new.savior_nick);
    end if;
    if (new.room_head is null) then
        if not (new.room_id is null) then
            new.room_head =
                (select doc.head
                    from doc
                        where doc.id = new.room_id );
            new.roomtype_id =
                (select room.roomtype_id
                    from room
                        where room.doc_id = new.room_id );
            new.roomtype_alias =
                (select room.roomtype_alias
                    from room
                        where room.doc_id = new.room_id );
        else
            new.room_head        = null;
        end if;
    end if;
    if (new.room_id is null) then
        new.room_id =
            (select doc.id
                from doc
                    where doc.head = new.room_head );
        new.roomtype_id =
            (select room.roomtype_id
                from room
                    where room.doc_id = new.room_id );
        new.roomtype_alias =
            (select room.roomtype_alias
                from room
                    where room.doc_id = new.room_id );
    end if;
    update pers set pers.live_room_id = new.room_id where pers.id = new.pers_id;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1exile_util_fields_on_insert on exile ;
create trigger t1exile_util_fields_on_insert before insert
on exile for each row execute procedure exile_util_fields_on_insert();

create or replace function exile_util_fields_on_update() returns "trigger" as $$
begin
    if new.pers_id != old.pers_id then
        new.pers_nick =
            (select pers.nick from pers where pers.id = new.pers_id);
    end if;
    if new.pers_nick != old.pers_nick then
        new.pers_id =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;
    if new.sender_id != old.sender_id then
        new.sender_nick =
            (select pers.nick from pers where pers.id = new.sender_id);
    end if;
    if new.sender_nick != old.sender_nick then
        new.sender_id =
            (select pers.id from pers where pers.nick = new.sender_nick);
    end if;
    if new.savior_id != old.savior_id then
        new.savior_nick =
            (select pers.nick from pers where pers.id = new.savior_id);
    end if;
    if new.savior_nick != old.savior_nick then
        new.savior_id =
            (select pers.id from pers where pers.nick = new.savior_nick);
    end if;

    if (new.isdeleted == true) then
        new.room_id = noobsroom();
    end if;

    if new.room_id != old.room_id then
        new.room_head =
            (select doc.head
                from doc
                    where doc.id = new.room_id );
        new.roomtype_id =
            (select room.roomtype_id
                from room
                    where room.doc_id = new.room_id );
        new.roomtype_alias =
            (select room.roomtype_alias
                from room
                    where room.doc_id = new.room_id );
    end if;
    if new.room_head != old.room_head then
        new.room_id =
            (select doc.id
                from doc
                    where doc.head = new.room_head );
        new.roomtype_id =
            (select room.roomtype_id
                from room
                    where room.doc_id = new.room_id );
        new.roomtype_alias =
            (select room.roomtype_alias
                from room
                    where room.doc_id = new.room_id );
    end if;
    
    update pers set pers.live_room_id = new.room_id where pers.id = new.pers_id;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1exile_util_fields_on_update on exile ;
create trigger t1exile_util_fields_on_update before update
on exile for each row execute procedure exile_util_fields_on_update();

\echo :FILE ok
