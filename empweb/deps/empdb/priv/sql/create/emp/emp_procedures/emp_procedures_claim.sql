\echo :FILE 'in'

create or replace function claim_util_fields_on_insert() returns "trigger" as $$
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

    if (new.judge_nick is null) then
        if not (new.judge_id is null) then
            new.judge_nick =
                (select pers.nick from pers where pers.id = new.judge_id);
        else
            new.judge_nick        = null;
        end if;
    end if;
    if (new.judge_id is null) then
        new.judge_id           =
            (select pers.id from pers where pers.nick = new.judge_nick);
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1claim_util_fields_on_insert on claim ;
create trigger t1claim_util_fields_on_insert before insert
on claim for each row execute procedure claim_util_fields_on_insert();

create or replace function claim_util_fields_on_update() returns "trigger" as $$
begin
    if new.pers_id != old.pers_id then
        new.pers_nick =
            (select pers.nick from pers where pers.id = new.pers_id);
    end if;
    if new.pers_nick != old.pers_nick then
        new.pers_id =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if new.judge_id != old.judge_id then
        new.judge_nick =
            (select pers.nick from pers where pers.id = new.judge_id);
    end if;
    if new.judge_nick != old.judge_nick then
        new.judge_id =
            (select pers.id from pers where pers.nick = new.judge_nick);
    end if;
    
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1claim_util_fields_on_update on claim ;
create trigger t1claim_util_fields_on_update before update
on claim for each row execute procedure claim_util_fields_on_update();

\echo :FILE ok
