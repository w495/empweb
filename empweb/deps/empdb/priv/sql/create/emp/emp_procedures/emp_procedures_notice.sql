\echo :FILE 'in'


--(2012.11.29 12:55:38:689927703)---------------------------------------------


/**
    @doc Возвращает начальное состояние документа
**/
create or replace function notice_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Владелец документа
    **/
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


    if (not (new.noticetype_id is null))
        and (new.noticetype_alias is null) then
        new.noticetype_alias =
            (select noticetype.alias
                from
                    noticetype
                where
                    noticetype.id = new.noticetype_id);
    end if;
    if (new.noticetype_id is null)
        and (not (new.noticetype_alias is null)) then
        new.noticetype_id =
            (select noticetype.id
                from
                    noticetype
                where
                    noticetype.alias = new.noticetype_alias);
    end if;

    return new;
end;
$$ language plpgsql;


drop trigger if exists t1notice_util_fields_on_insert on notice ;
create trigger t1notice_util_fields_on_insert before insert
on notice for each row execute procedure notice_util_fields_on_insert();

/**
    @doc Обеспечивает совместное состояние документа
**/
create or replace function notice_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Владелец документа
    **/
    if new.pers_id != old.pers_id then
        new.pers_nick =
            (select pers.nick from pers where pers.id = new.pers_id);
    end if;
    if new.pers_nick != old.pers_nick then
        new.pers_id =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if new.noticetype_id != old.noticetype_id then
        new.noticetype_alias =
            (select noticetype.alias
                from
                    noticetype
                where
                    noticetype.id = new.noticetype_id);
    end if;
    if new.noticetype_alias != old.noticetype_alias then
        new.noticetype_id =
            (select noticetype.id
                from
                    noticetype
                where
                    noticetype.alias = new.noticetype_alias);
    end if;


    return new;
end;
$$ language plpgsql;

drop trigger if exists t1notice_util_fields_on_update on notice ;
create trigger t1notice_util_fields_on_update before update
on notice for each row execute procedure notice_util_fields_on_update();


\echo :FILE ok
