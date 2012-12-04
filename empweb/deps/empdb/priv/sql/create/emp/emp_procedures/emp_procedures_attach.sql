\echo :FILE 'in'




/**
    @doc Обеспечивает совместное состояние аттача
**/
create or replace function attach_util_fields_on_update() returns "trigger" as $$
begin
    /**
    *  Типы сообщества
    **/
    if new.attachtype_id != old.attachtype_id then
        new.attachtype_alias =
            (select attachtype.alias
                from
                    attachtype
                where
                    attachtype.id = new.attachtype_id);
    end if;
    if new.attachtype_alias != old.attachtype_alias then
        new.attachtype_id =
            (select attachtype.id
                from
                    attachtype
                where
                    attachtype.alias = new.attachtype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1attach_util_fields_on_update on attach ;
create trigger t1attach_util_fields_on_update before update
on attach for each row execute procedure attach_util_fields_on_update();


create or replace function attach_util_fields_on_insert() returns "trigger" as $$
begin
    /**
    *  Типы аттачей
    **/
    if (not (new.attachtype_id is null)) and (new.attachtype_alias is null) then
        new.attachtype_alias =
            (select attachtype.alias
                from
                    attachtype
                where
                    attachtype.id = new.attachtype_id);
    end if;
    if (new.attachtype_id is null) and (not (new.attachtype_alias is null)) then
        new.attachtype_id =
            (select attachtype.id
                from
                    attachtype
                where
                    attachtype.alias = new.attachtype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1attach_util_fields_on_insert on attach ;
create trigger t1attach_util_fields_on_insert before insert
on attach for each row execute procedure attach_util_fields_on_insert();



\echo :FILE ok
