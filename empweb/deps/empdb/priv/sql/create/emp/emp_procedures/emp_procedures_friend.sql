\echo :FILE 'in'

--(2012.11.22 16:00:30:419259748)---------------------------------------------

/**
    @doc Возвращает начальное состояние документа
**/
create or replace function friend_util_fields_on_insert() returns "trigger" as $$
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

    if (new.friend_nick is null) then
        if not (new.friend_id is null) then
            new.friend_nick =
                (select pers.nick from pers where pers.id = new.friend_id);
        else
            new.friend_nick        = null;
        end if;
    end if;
    if (new.friend_id is null) then
        new.friend_id           =
            (select pers.id from pers where pers.nick = new.friend_nick);
    end if;


    if (new.friendtype_alias is null) then
        if not (new.friendtype_id is null) then
            new.friendtype_alias =
                (select friendtype.alias
                    from friendtype
                        where friendtype.id = new.friendtype_id);
        end if;
    end if;
    if (new.friendtype_id is null) then
        new.friendtype_id           =
            (select friendtype.id
                from friendtype
                    where friendtype.alias = new.friendtype_alias);
    end if;

    return new;
end;
$$ language plpgsql;


drop trigger if exists t1friend_util_fields_on_insert on friend ;
create trigger t1friend_util_fields_on_insert before insert
on friend for each row execute procedure friend_util_fields_on_insert();

/**
    @doc Обеспечивает совместное состояние документа
**/
create or replace function friend_util_fields_on_update() returns "trigger" as $$
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

    if new.friend_id != old.friend_id then
        new.friend_nick =
            (select pers.nick from pers where pers.id = new.friend_id);
    end if;
    if new.friend_nick != old.friend_nick then
        new.friend_id =
            (select pers.id from pers where pers.nick = new.friend_nick);
    end if;


    if new.friendtype_id != old.friendtype_id then
        new.friendtype_alias =
            (select friendtype.alias
                from friendtype
                    where friendtype.id = new.friendtype_id);
    end if;
    if new.friendtype_alias != old.friendtype_alias then
        new.friendtype_id =
            (select friendtype.id
                from friendtype
                    where friendtype.alias = new.friendtype_alias);
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1friend_util_fields_on_update on friend ;
create trigger t1friend_util_fields_on_update before update
on friend for each row execute procedure friend_util_fields_on_update();


\echo :FILE ok
