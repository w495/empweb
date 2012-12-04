\echo :FILE 'in'




/**
    --------------------------------------------------------------------------
        @doc Обеспечивает совместное состояние cooбщения
    --------------------------------------------------------------------------
**/


create or replace function message_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Получатель сообщения
    **/
    if (new.reader_nick is null) then
        if not (new.reader_id is null) then
            new.reader_nick =
                (select pers.nick from pers where pers.id = new.reader_id);
        else
            new.reader_nick        = null;
        end if;
    end if;
    if (new.reader_id is null) then
        new.reader_id           =
            (select pers.id from pers where pers.nick = new.reader_nick);
    end if;

    /**
    *  Типы сообщества
    **/
    if (not (new.messagetype_id is null))
        and (new.messagetype_alias is null) then
        new.messagetype_alias =
            (select messagetype.alias
                from
                    messagetype
                where
                    messagetype.id = new.messagetype_id);
    end if;
    if (new.messagetype_id is null)
        and (not (new.messagetype_alias is null)) then
        new.messagetype_id =
            (select messagetype.id
                from
                    messagetype
                where
                    messagetype.alias = new.messagetype_alias);
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1message_util_fields_on_insert on message ;
create trigger t1message_util_fields_on_insert before insert
on message for each row execute procedure message_util_fields_on_insert();

create or replace function message_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Получатель сообщения
    **/
    if new.reader_id != old.reader_id then
        new.reader_nick =
            (select pers.nick from pers where pers.id = new.reader_id);
    end if;
    if new.reader_nick != old.reader_nick then
        new.reader_id =
            (select pers.id from pers where pers.nick = new.reader_nick);
    end if;
    /**
    *  Типы cooбщения
    **/
    if new.messagetype_id != old.messagetype_id then
        new.messagetype_alias =
            (select messagetype.alias
                from
                    messagetype
                where
                    messagetype.id = new.messagetype_id);
    end if;
    if new.messagetype_alias != old.messagetype_alias then
        new.messagetype_id =
            (select messagetype.id
                from
                    messagetype
                where
                    messagetype.alias = new.messagetype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1message_util_fields_on_update on message ;
create trigger t1message_util_fields_on_update before update
on message for each row execute procedure message_util_fields_on_update();




\echo :FILE ok
