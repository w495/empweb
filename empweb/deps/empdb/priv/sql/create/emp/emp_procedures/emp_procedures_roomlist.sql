\echo :FILE 'in'

create or replace function roomlist_util_fields_on_insert() returns "trigger" as $$
begin
    /**
    *  Типы жалоб
    **/
    if (not (new.roomlisttype_id is null))
        and (new.roomlisttype_alias is null) then
        new.roomlisttype_alias =
            (select roomlisttype.alias
                from
                    roomlisttype
                where
                    roomlisttype.id = new.roomlisttype_id);
    end if;
    if (new.roomlisttype_id is null)
        and (not (new.roomlisttype_alias is null)) then
        new.roomlisttype_id =
            (select roomlisttype.id
                from
                    roomlisttype
                where
                    roomlisttype.alias = new.roomlisttype_alias);
    end if;

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

    if (new.room_head is null) then
        if not (new.room_id is null) then
            new.room_head =
                (select doc.head
                    from doc
                        where doc.id = new.room_id
                            and doc.doctype_alias='room');
        else
            new.room_head        = null;
        end if;
    end if;
    if (new.room_id is null) then
        new.room_id =
            (select doc.id
                from doc
                    where doc.head = new.room_head
                        and doc.doctype_alias='room');
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roomlist_util_fields_on_insert on roomlist ;
create trigger t1roomlist_util_fields_on_insert before insert
on roomlist for each row execute procedure roomlist_util_fields_on_insert();

create or replace function roomlist_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Типы жалоб
    **/
    if new.roomlisttype_id != old.roomlisttype_id then
        new.roomlisttype_alias =
            (select roomlisttype.alias
                from
                    roomlisttype
                where
                    roomlisttype.id = new.roomlisttype_id);
    end if;
    if new.roomlisttype_alias != old.roomlisttype_alias then
        new.roomlisttype_id =
            (select roomlisttype.id
                from
                    roomlisttype
                where
                    roomlisttype.alias = new.roomlisttype_alias);
    end if;
    
    if new.pers_id != old.pers_id then
        new.pers_nick =
            (select pers.nick from pers where pers.id = new.pers_id);
    end if;
    if new.pers_nick != old.pers_nick then
        new.pers_id =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if new.room_id != old.room_id then
        new.room_head =
            (select doc.head
                from doc
                    where doc.id = new.room_id and doc.doctype_alias='room');
    end if;
    if new.room_head != old.room_head then
        new.room_id =
            (select doc.id
                from doc
                    where doc.head = new.room_head and doc.doctype_alias='room');
    end if;
    
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roomlist_util_fields_on_update on roomlist ;
create trigger t1roomlist_util_fields_on_update before update
on roomlist for each row execute procedure roomlist_util_fields_on_update();

\echo :FILE ok
