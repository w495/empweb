\echo :FILE 'in'



/**
    @doc Возвращает начальное состояние документа
**/
create or replace function fileinfo_util_fields_on_insert() returns "trigger" as $$
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

    if (new.filetype_alias is null) then
        if not (new.filetype_id is null) then
            new.filetype_alias =
                (select filetype.alias from filetype
                    where filetype.id = new.filetype_id);
        else
            new.filetype_alias        = null;
        end if;
    end if;
    if (new.filetype_id is null) then
        new.filetype_id           =
            (select filetype.id from filetype
                where filetype.alias = new.filetype_alias);
    end if;

    if (new.fileinfotype_alias is null) then
        if not (new.fileinfotype_id is null) then
            new.fileinfotype_alias =
                (select fileinfotype.alias from fileinfotype
                    where fileinfotype.id = new.fileinfotype_id);
        else
            new.fileinfotype_alias        = null;
        end if;
    end if;
    if (new.fileinfotype_id is null) then
        new.fileinfotype_id           =
            (select fileinfotype.id from fileinfotype
                where fileinfotype.alias = new.fileinfotype_alias);
    end if;

    return new;
end;
$$ language plpgsql;


drop trigger if exists t1fileinfo_util_fields_on_insert on fileinfo ;
create trigger t1fileinfo_util_fields_on_insert before insert
on fileinfo for each row execute procedure fileinfo_util_fields_on_insert();

/**
    @doc Обеспечивает совместное состояние документа
**/
create or replace function fileinfo_util_fields_on_update() returns "trigger" as $$
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

    if new.filetype_id != old.filetype_id then
        new.filetype_alias =
            (select filetype.alias from filetype
                where filetype.id = new.filetype_id);
    end if;
    if new.filetype_alias != old.filetype_alias then
        new.filetype_id =
            (select filetype.id from filetype
                where filetype.alias = new.filetype_alias);
    end if;

    if new.fileinfotype_id != old.fileinfotype_id then
        new.fileinfotype_alias =
            (select fileinfotype.alias from fileinfotype
                where fileinfotype.id = new.fileinfotype_id);
    end if;
    if new.fileinfotype_alias != old.fileinfotype_alias then
        new.fileinfotype_id =
            (select fileinfotype.id from fileinfotype
                where fileinfotype.alias = new.fileinfotype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1fileinfo_util_fields_on_update on fileinfo ;
create trigger t1fileinfo_util_fields_on_update before update
on fileinfo for each row execute procedure fileinfo_util_fields_on_update();





\echo :FILE ok
