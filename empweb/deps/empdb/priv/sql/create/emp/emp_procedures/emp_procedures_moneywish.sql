\echo :FILE 'in'



--(2012.10.12 14:38:23:554386209)---------------------------------------------


/**
    @doc Обеспечивает совместное состояние покупок авторитетов (moneywish)
**/
create or replace function moneywish_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Владелец покупки
    **/
    if (new.owner_nick is null) then
        if not (new.owner_id is null) then
            new.owner_nick =
                (select pers.nick from pers where pers.id = new.owner_id);
        else
            /**
                если владелец не указан, то им становится, тот кто покупает
            **/
            new.owner_nick        = new.wisher_nick;
        end if;
    end if;
    if (new.owner_id is null) then
        new.owner_id           =
            (select pers.id from pers where pers.nick = new.owner_nick);
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1moneywish_util_fields_on_insert on moneywish ;
create trigger t1moneywish_util_fields_on_insert before insert
on moneywish for each row execute procedure moneywish_util_fields_on_insert();

create or replace function moneywish_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Плательщик покупки
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

drop trigger if exists t1moneywish_util_fields_on_update on moneywish ;
create trigger t1moneywish_util_fields_on_update before update
on moneywish for each row execute procedure moneywish_util_fields_on_update();




\echo :FILE ok
