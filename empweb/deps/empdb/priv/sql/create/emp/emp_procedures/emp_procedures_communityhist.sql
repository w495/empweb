\echo :FILE 'in'


--(2012.11.29 12:55:38:689927703)---------------------------------------------

/**
    @doc Возвращает начальное состояние документа
**/
create or replace function communityhist_util_fields_on_insert() returns "trigger" as $$
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


    if (not (new.communityhisttype_id is null))
        and (new.communityhisttype_alias is null) then
        new.communityhisttype_alias =
            (select communityhisttype.alias
                from
                    communityhisttype
                where
                    communityhisttype.id = new.communityhisttype_id);
    end if;
    if (new.communityhisttype_id is null)
        and (not (new.communityhisttype_alias is null)) then
        new.communityhisttype_id =
            (select communityhisttype.id
                from
                    communityhisttype
                where
                    communityhisttype.alias = new.communityhisttype_alias);
    end if;

    return new;
end;
$$ language plpgsql;


drop trigger if exists t1communityhist_util_fields_on_insert on communityhist ;
create trigger t1communityhist_util_fields_on_insert before insert
on communityhist for each row execute procedure communityhist_util_fields_on_insert();

/**
    @doc Обеспечивает совместное состояние документа
**/
create or replace function communityhist_util_fields_on_update() returns "trigger" as $$
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

    if new.communityhisttype_id != old.communityhisttype_id then
        new.communityhisttype_alias =
            (select communityhisttype.alias
                from
                    communityhisttype
                where
                    communityhisttype.id = new.communityhisttype_id);
    end if;
    if new.communityhisttype_alias != old.communityhisttype_alias then
        new.communityhisttype_id =
            (select communityhisttype.id
                from
                    communityhisttype
                where
                    communityhisttype.alias = new.communityhisttype_alias);
    end if;


    return new;
end;
$$ language plpgsql;

drop trigger if exists t1communityhist_util_fields_on_update on communityhist ;
create trigger t1communityhist_util_fields_on_update before update
on communityhist for each row execute procedure communityhist_util_fields_on_update();

\echo :FILE ok
