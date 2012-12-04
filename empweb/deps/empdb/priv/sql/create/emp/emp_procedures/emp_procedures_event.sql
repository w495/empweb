\echo :FILE 'in'



/**
    @doc Обеспечивает совместное состояние события
**/

create or replace function event_util_fields_on_update() returns "trigger" as $$
begin
    /**
    *  Типы сообщества
    **/
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
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1event_util_fields_on_insert on event ;
create trigger t1event_util_fields_on_insert before insert
on event for each row execute procedure event_util_fields_on_insert();




\echo :FILE ok
