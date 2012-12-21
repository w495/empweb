\echo :FILE 'in'



--(2012.11.05 22:40:59:888957862)---------------------------------------------

/**
    @doc Обеспечивает совместное состояние лога прихода\расхода страны
**/
create or replace function communitytreas_util_fields_on_insert() returns "trigger" as $$
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

    if (new.treastype_alias is null) then
        if not (new.treastype_id is null) then
            new.treastype_alias =
                (select treastype.alias
                    from treastype
                        where treastype.id = new.treastype_id);
        end if;
    end if;
    if (new.treastype_id is null) then
        new.treastype_id           =
            (select treastype.id
                from treastype
                    where treastype.alias = new.treastype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1communitytreas_util_fields_on_insert on communitytreas ;
create trigger t1communitytreas_util_fields_on_insert before insert
on communitytreas for each row execute procedure communitytreas_util_fields_on_insert();

create or replace function communitytreas_util_fields_on_update() returns "trigger" as $$
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

    if new.treastype_id != old.treastype_id then
        new.treastype_alias =
            (select treastype.alias
                from treastype
                    where treastype.id = new.treastype_id);
    end if;
    if new.treastype_alias != old.treastype_alias then
        new.treastype_id =
            (select treastype.id
                from treastype
                    where treastype.nick = new.treastype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1communitytreas_util_fields_on_update on communitytreas ;
create trigger t1communitytreas_util_fields_on_update before update
on communitytreas for each row execute procedure communitytreas_util_fields_on_update();




\echo :FILE ok
