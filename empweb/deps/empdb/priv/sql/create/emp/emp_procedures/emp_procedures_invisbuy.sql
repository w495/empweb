\echo :FILE 'in'

create or replace function invisbuy_util_fields_on_insert() returns "trigger" as $$
begin
    if (new.owner_nick is null) then
        if not (new.owner_id is null) then
            new.owner_nick =
                (select pers.nick from pers where pers.id = new.owner_id);
        else
            new.owner_nick        = null;
        end if;
    end if;
    if (new.owner_id is null) then
        new.owner_id           =
            (select pers.id from pers where pers.nick = new.owner_nick);
    end if;
    if (new.buyer_nick is null) then
        if not (new.buyer_id is null) then
            new.buyer_nick =
                (select pers.nick from pers where pers.id = new.buyer_id);
        else
            new.buyer_nick        = null;
        end if;
    end if;
    if (new.buyer_id is null) then
        new.buyer_id           =
            (select pers.id from pers where pers.nick = new.buyer_nick);
    end if;
    if (new.invistype_alias is null) then
        if not (new.invistype_id is null) then
            new.invistype_alias =
                (select invistype.alias
                    from invistype
                        where invistype.id = new.invistype_id);
            new.invistype_level =
                (select invistype.level
                    from invistype
                        where invistype.id = new.invistype_id);
        else
            new.invistype_alias      = null;
            new.invistype_level     = null;
        end if;
    end if;
    if (new.invistype_id is null) then
        new.invistype_id =
            (select invistype.id
                from invistype
                    where invistype.alias = new.invistype_alias);
        new.invistype_level =
            (select invistype.level
                from invistype
                    where invistype.alias = new.invistype_alias);
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1invisbuy_util_fields_on_insert on invisbuy ;
create trigger t1invisbuy_util_fields_on_insert before insert
on invisbuy for each row execute procedure invisbuy_util_fields_on_insert();

create or replace function invisbuy_util_fields_on_update() returns "trigger" as $$
begin
    if new.owner_id != old.owner_id then
        new.owner_nick =
            (select pers.nick from pers where pers.id = new.owner_id);
    end if;
    if new.owner_nick != old.owner_nick then
        new.owner_id =
            (select pers.id from pers where pers.nick = new.owner_nick);
    end if;
    if new.buyer_id != old.buyer_id then
        new.buyer_nick =
            (select pers.nick from pers where pers.id = new.buyer_id);
    end if;
    if new.buyer_nick != old.buyer_nick then
        new.buyer_id =
            (select pers.id from pers where pers.nick = new.buyer_nick);
    end if;
    if new.invistype_id != old.invistype_id then
        new.invistype_alias =
            (select invistype.alias
                from invistype
                    where invistype.id = new.invistype_id);
            new.invistype_level =
                (select invistype.level
                    from invistype
                        where invistype.id = new.invistype_id);
    end if;
    if new.invistype_alias != old.invistype_alias then
        new.invistype_id =
            (select invistype.id
                from invistype
                    where invistype.alias = new.invistype_alias);
        new.invistype_level =
            (select invistype.level
                from invistype
                    where invistype.alias = new.invistype_alias);
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1invisbuy_util_fields_on_update on invisbuy ;
create trigger t1invisbuy_util_fields_on_update before update
on invisbuy for each row execute procedure invisbuy_util_fields_on_update();

\echo :FILE ok

