\echo :FILE 'in'

create or replace function repost_util_fields_on_insert() returns "trigger" as $$
begin
    if (new.owner_nick is null) then
        if not (new.owner_id is null) then
            new.owner_nick =
                (select owner.nick from owner where owner.id = new.owner_id);
        else
            new.owner_nick        = null;
        end if;
    end if;
    if (new.owner_id is null) then
        new.owner_id           =
            (select owner.id from owner where owner.nick = new.owner_nick);
    end if;

    if (new.orig_owner_nick is null) then
        if not (new.orig_owner_id is null) then
            new.orig_owner_nick =
                (select owner.nick from owner where owner.id = new.orig_owner_id);
        else
            new.orig_owner_nick        = null;
        end if;
    end if;
    if (new.orig_owner_id is null) then
        new.orig_owner_id           =
            (select owner.id from owner where owner.nick = new.orig_owner_nick);
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1repost_util_fields_on_insert on repost ;
create trigger t1repost_util_fields_on_insert before insert
on repost for each row execute procedure repost_util_fields_on_insert();

create or replace function repost_util_fields_on_update() returns "trigger" as $$
begin
    if new.owner_id != old.owner_id then
        new.owner_nick =
            (select owner.nick from owner where owner.id = new.owner_id);
    end if;
    if new.owner_nick != old.owner_nick then
        new.owner_id =
            (select owner.id from owner where owner.nick = new.owner_nick);
    end if;

    if new.orig_owner_id != old.orig_owner_id then
        new.orig_owner_nick =
            (select owner.nick from owner where owner.id = new.orig_owner_id);
    end if;
    if new.orig_owner_nick != old.orig_owner_nick then
        new.orig_owner_id =
            (select owner.id from owner where owner.nick = new.orig_owner_nick);
    end if;
    
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1repost_util_fields_on_update on repost ;
create trigger t1repost_util_fields_on_update before update
on repost for each row execute procedure repost_util_fields_on_update();

\echo :FILE ok
