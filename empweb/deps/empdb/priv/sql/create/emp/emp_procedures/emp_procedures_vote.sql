\echo :FILE 'in'




--(2012.10.25 18:19:03:973218989)---------------------------------------------

/**
    @doc Обеспечивает совместное состояние ставки аукциона
**/
create or replace function vote_util_fields_on_insert() returns "trigger" as $$
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


    update doc set nvotes = nvotes + 1 where id = new.doc_id;
    
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1vote_util_fields_on_insert on vote ;
create trigger t1vote_util_fields_on_insert before insert
on vote for each row execute procedure vote_util_fields_on_insert();

create or replace function vote_util_fields_on_update() returns "trigger" as $$
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

    if new.isdeleted != old.isdeleted then
        if (new.isdeleted = true) then
            update doc set nvotes = nvotes - 1 where id = new.doc_id;
        else
            update doc set nvotes = nvotes + 1 where id = new.doc_id;
        end if;
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1vote_util_fields_on_update on vote ;
create trigger t1vote_util_fields_on_update before update
on vote for each row execute procedure vote_util_fields_on_update();


create or replace function vote_util_fields_on_delete() returns "trigger" as $$
begin

    if (old.isdeleted = true) then
        update doc set nvotes = nvotes - 1 where id = old.doc_id;
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1vote_util_fields_on_delete on vote ;
create trigger t1vote_util_fields_on_delete before delete
on vote for each row execute procedure vote_util_fields_on_delete();


\echo :FILE ok
