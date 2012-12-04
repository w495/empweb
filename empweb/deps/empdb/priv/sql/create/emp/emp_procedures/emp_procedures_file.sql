\echo :FILE 'in'



--(2012.11.20 15:13:30:800771012)---------------------------------------------

/**
    @doc Возвращает начальное состояние документа
**/
create or replace function file_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Владелец документа
    **/
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

    return new;
end;
$$ language plpgsql;


drop trigger if exists t1file_util_fields_on_insert on file ;
create trigger t1file_util_fields_on_insert before insert
on file for each row execute procedure file_util_fields_on_insert();

/**
    @doc Обеспечивает совместное состояние документа
**/
create or replace function file_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Владелец документа
    **/
    if new.owner_id != old.owner_id then
        new.owner_nick =
            (select pers.nick from pers where pers.id = new.owner_id);
    end if;
    if new.owner_nick != old.owner_nick then
        new.owner_id =
            (select pers.id from pers where pers.nick = new.owner_nick);
    end if;


    return new;
end;
$$ language plpgsql;

drop trigger if exists t1file_util_fields_on_update on file ;
create trigger t1file_util_fields_on_update before update
on file for each row execute procedure file_util_fields_on_update();





\echo :FILE ok
