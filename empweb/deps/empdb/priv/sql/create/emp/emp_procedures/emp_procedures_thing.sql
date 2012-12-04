\echo :FILE 'in'




/**
    @doc Обеспечивает совместное состояние вещей (thing)
**/

create or replace function thing_util_fields_on_update() returns "trigger" as $$
begin
    if new.thingtype_id != old.thingtype_id then
        update thingtype set
            nchildtargets   = nchildtargets + 1
        where
            thingtype.id = new.thingtype_id;
        update thingtype set
            nchildtargets   = nchildtargets - 1
        where
            thingtype.id = new.thingtype_id;
    end if;
    if (new.isdeleted = true) and (old.isdeleted = false) then
        update thingtype set
            nchildtargets   = nchildtargets - 1
        where
            thingtype.id = new.thingtype_id;
    end if;
    if (new.isdeleted = false) and (old.isdeleted = true) then
        update thingtype set
            nchildtargets   = nchildtargets + 1
        where
            thingtype.id = new.thingtype_id;
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1thing_util_fields_on_update on thing ;
create trigger t1thing_util_fields_on_update before update
on thing for each row execute procedure thing_util_fields_on_update();


create or replace function thing_util_fields_on_insert() returns "trigger" as $$
begin
    if not (new.thingtype_id is null) then
        update thingtype set
            nchildtargets   = nchildtargets + 1
        where
            thingtype.id = new.thingtype_id;
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1thing_util_fields_on_insert on thing ;
create trigger t1thing_util_fields_on_insert before insert
on thing for each row execute procedure thing_util_fields_on_insert();


create or replace function thing_util_fields_on_delete() returns "trigger" as $$
begin
    if not (old.thingtype_id is null) then
        update thingtype set
            nchildtargets   = nchildtargets - 1
        where
            thingtype.id = old.thingtype_id;
    end if;
    return old;
end;
$$ language plpgsql;


drop trigger if exists t1thing_util_fields_on_delete on thing ;
create trigger t1thing_util_fields_on_delete before delete
on thing for each row execute procedure thing_util_fields_on_delete();



\echo :FILE ok
