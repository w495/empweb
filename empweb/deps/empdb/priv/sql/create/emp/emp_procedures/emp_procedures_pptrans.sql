\echo :FILE 'in'



/**
    @doc Обеспечивает совместное состояние денежного перевода для страны
**/
create or replace function pptrans_util_fields_on_insert() returns "trigger" as $$
begin
    if (new.pers_nick is null) then
        if not (new.pers_id is null) then
            new.pers_nick =
                (select pers.nick from pers where pers.id = new.pers_id);
        end if;
    end if;
    if (new.pers_id is null) then
        new.pers_id           =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if (new.transtype_alias is null) then
        if not (new.transtype_id is null) then
            new.transtype_alias =
                (select transtype.alias
                    from transtype
                        where transtype.id = new.transtype_id);
        end if;
    end if;
    if (new.transtype_id is null) then
        new.transtype_id           =
            (select transtype.id
                from transtype
                    where transtype.alias = new.transtype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1pptrans_util_fields_on_insert on pptrans ;
create trigger t1pptrans_util_fields_on_insert before insert
on pptrans for each row execute procedure pptrans_util_fields_on_insert();

create or replace function pptrans_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Владелец заявки
    **/
    if new.pers_id != old.pers_id then
        new.pers_nick =
            (select pers.nick from pers where pers.id = new.pers_id);
    end if;
    if new.pers_nick != old.pers_nick then
        new.pers_id =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if new.transtype_id != old.transtype_id then
        new.transtype_alias =
            (select transtype.alias
                from transtype
                    where transtype.id = new.transtype_id);
    end if;
    if new.transtype_alias != old.transtype_alias then
        new.transtype_id =
            (select transtype.id
                from transtype
                    where transtype.nick = new.transtype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1pptrans_util_fields_on_update on pptrans ;
create trigger t1pptrans_util_fields_on_update before update
on pptrans for each row execute procedure pptrans_util_fields_on_update();




\echo :FILE ok
