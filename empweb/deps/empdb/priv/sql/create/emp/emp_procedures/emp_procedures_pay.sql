\echo :FILE 'in'




--(2012.10.25 18:19:03:973218989)---------------------------------------------

/**
    @doc Обеспечивает совместное состояние ставки аукциона
**/
create or replace function pay_util_fields_on_insert() returns "trigger" as $$
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

    if (new.paytype_alias is null) then
        if not (new.paytype_id is null) then
            new.paytype_alias =
                (select paytype.alias
                    from paytype
                        where paytype.id = new.paytype_id);
        end if;
    end if;
    if (new.paytype_id is null) then
        new.paytype_id           =
            (select paytype.id
                from paytype
                    where paytype.alias = new.paytype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1pay_util_fields_on_insert on pay ;
create trigger t1pay_util_fields_on_insert before insert
on pay for each row execute procedure pay_util_fields_on_insert();

create or replace function pay_util_fields_on_update() returns "trigger" as $$
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

    if new.paytype_id != old.paytype_id then
        new.paytype_alias =
            (select paytype.alias
                from paytype
                    where paytype.id = new.paytype_id);
    end if;
    if new.paytype_alias != old.paytype_alias then
        new.paytype_id =
            (select paytype.id
                from paytype
                    where paytype.nick = new.paytype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1pay_util_fields_on_update on pay ;
create trigger t1pay_util_fields_on_update before update
on pay for each row execute procedure pay_util_fields_on_update();



\echo :FILE ok
