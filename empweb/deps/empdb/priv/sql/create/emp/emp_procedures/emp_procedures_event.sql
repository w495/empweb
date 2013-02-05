\echo :FILE 'in'



/**
    @doc Обеспечивает совместное состояние события
**/

create or replace function event_util_fields_on_update() returns "trigger" as $$
begin
    if new.eventtype_id != old.eventtype_id then
        new.eventtype_alias =
            (select eventtype.alias
                from
                    eventtype
                where
                    eventtype.id = new.eventtype_id);
    end if;
    if new.eventtype_alias != old.eventtype_alias then
        new.eventtype_id =
            (select eventtype.id
                from
                    eventtype
                where
                    eventtype.alias = new.eventtype_alias);
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
    
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1event_util_fields_on_update on event ;
create trigger t1event_util_fields_on_update before update
on event for each row execute procedure event_util_fields_on_update();


create or replace function event_util_fields_on_insert() returns "trigger" as $$
begin
    /**
    *  Типы событий
    **/
    if (not (new.eventtype_id is null)) and (new.eventtype_alias is null) then
        new.eventtype_alias =
            (select eventtype.alias
                from
                    eventtype
                where
                    eventtype.id = new.eventtype_id);
    end if;
    if (new.eventtype_id is null) and (not (new.eventtype_alias is null)) then
        new.eventtype_id =
            (select eventtype.id
                from
                    eventtype
                where
                    eventtype.alias = new.eventtype_alias);
    end if;
    if new.pers_id != old.pers_id then
        new.pers_nick =
            (select pers.nick from pers where pers.id = new.pers_id);
    end if;
    if new.pers_nick != old.pers_nick then
        new.pers_id =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1event_util_fields_on_insert on event ;
create trigger t1event_util_fields_on_insert before insert
on event for each row execute procedure event_util_fields_on_insert();




\echo :FILE ok
