\echo :FILE 'in'

create or replace function action_util_fields_on_insert() returns "trigger" as $$
begin
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

    if (new.actiontype_nick is null) then
        if not (new.actiontype_id is null) then
            new.actiontype_nick =
                (select actiontype.nick
                    from actiontype
                        where actiontype.id = new.actiontype_id);
        else
            new.actiontype_nick        = null;
        end if;
    end if;
    if (new.actiontype_id is null) then
        new.actiontype_id           =
            (select actiontype.id
                from actiontype
                    where actiontype.nick = new.actiontype_nick);
    end if;
    
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1action_util_fields_on_insert on action ;
create trigger t1action_util_fields_on_insert before insert
on action for each row execute procedure action_util_fields_on_insert();

create or replace function action_util_fields_on_update() returns "trigger" as $$
begin
    if new.pers_id != old.pers_id then
        new.pers_nick =
            (select pers.nick from pers where pers.id = new.pers_id);
    end if;
    if new.pers_nick != old.pers_nick then
        new.pers_id =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if new.owner_id != old.owner_id then
        new.owner_nick =
            (select pers.nick from pers where pers.id = new.owner_id);
    end if;
    if new.owner_nick != old.owner_nick then
        new.owner_id =
            (select pers.id from pers where pers.nick = new.owner_nick);
    end if;

    if new.actiontype_id != old.actiontype_id then
        new.actiontype_nick =
            (select actiontype.nick
                from actiontype
                    where actiontype.id = new.actiontype_id);
    end if;
    if new.actiontype_nick != old.actiontype_nick then
        new.actiontype_id =
            (select actiontype.id
                from actiontype
                    where actiontype.nick = new.actiontype_nick);
    end if;

    
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1action_util_fields_on_update on action ;
create trigger t1action_util_fields_on_update before update
on action for each row execute procedure action_util_fields_on_update();

\echo :FILE ok
