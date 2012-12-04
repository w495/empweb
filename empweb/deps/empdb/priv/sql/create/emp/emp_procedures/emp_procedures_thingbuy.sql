\echo :FILE 'in'



--(2012.10.12 14:38:23:554386209)---------------------------------------------

/**
    @doc Обеспечивает совместное состояние покупок (thingbuy)
**/
create or replace function thingbuy_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Плательщик покупки
    **/
    if (new.buyer_nick is null) then
        if not (new.buyer_id is null) then
            new.buyer_nick =
                (select pers.nick from pers where pers.id = new.buyer_id);
        else
            new.buyer_nick        = null;
        end if;
    end if;
    if (new.buyer_id is null) then
        new.buyer_id           =
            (select pers.id from pers where pers.nick = new.buyer_nick);
    end if;
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
            new.owner_nick        = new.buyer_nick;
        end if;
    end if;
    if (new.owner_id is null) then
        new.owner_id           =
            (select pers.id from pers where pers.nick = new.owner_nick);
    end if;
    /**
        Покупаемая вещь
    **/
    if (new.thing_alias is null) then
        if not (new.thing_id is null) then
            new.thing_alias =
                (select thing.alias from thing where thing.id = new.thing_id);
        else
            new.thing_alias        = null;
        end if;
    end if;
    if (new.thing_id is null) then
        new.thing_id           =
            (select thing.id from thing where thing.alias = new.thing_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1thingbuy_util_fields_on_insert on thingbuy ;
create trigger t1thingbuy_util_fields_on_insert before insert
on thingbuy for each row execute procedure thingbuy_util_fields_on_insert();

create or replace function thingbuy_util_fields_on_update() returns "trigger" as $$
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
    /**
        Владелец покупки
    **/
    if new.buyer_id != old.buyer_id then
        new.buyer_nick =
            (select pers.nick from pers where pers.id = new.buyer_id);
    end if;
    if new.buyer_nick != old.buyer_nick then
        new.buyer_id =
            (select pers.id from pers where pers.nick = new.buyer_nick);
    end if;
    /**
        Покупаемая вещь
    **/
    if new.thing_id != old.thing_id then
        new.thing_alias =
            (select thing.alias from thing where thing.id = new.thing_id);
    end if;
    if new.thing_alias != old.thing_alias then
        new.thing_id =
            (select thing.id from thing where thing.alias = new.thing_alias);
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1thingbuy_util_fields_on_update on thingbuy ;
create trigger t1thingbuy_util_fields_on_update before update
on thingbuy for each row execute procedure thingbuy_util_fields_on_update();




\echo :FILE ok
