\echo :FILE 'in'



--(2012.10.12 14:38:23:554386209)---------------------------------------------

/**
    @doc Обеспечивает совместное состояние покупок (thingwish)
**/
create or replace function thingwish_util_fields_on_insert() returns "trigger" as $$
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
            new.thingtype_id =
                (select thing.thingtype_id from thing where thing.id = new.thing_id);
            new.thingtype_alias =
                (select thing.thingtype_alias from thing where thing.id = new.thing_id);
        else
            new.thing_alias        = null;
        end if;
    end if;
    if (new.thing_id is null) then
        new.thing_id           =
            (select thing.id from thing where thing.alias = new.thing_alias);
        new.thingtype_id =
            (select thing.thingtype_id from thing where thing.id = new.thing_id);
        new.thingtype_alias =
            (select thing.thingtype_alias from thing where thing.id = new.thing_id);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1thingwish_util_fields_on_insert on thingwish ;
create trigger t1thingwish_util_fields_on_insert before insert
on thingwish for each row execute procedure thingwish_util_fields_on_insert();

create or replace function thingwish_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Владелец покупки
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
        Покупаемая вещь
    **/
    if new.thing_id != old.thing_id then
        new.thing_alias =
            (select thing.alias from thing where thing.id = new.thing_id);
        new.thingtype_id =
            (select thing.thingtype_id from thing where thing.id = new.thing_id);
        new.thingtype_alias =
            (select thing.thingtype_alias from thing where thing.id = new.thing_id);
    end if;
    if new.thing_alias != old.thing_alias then
        new.thing_id =
            (select thing.id from thing where thing.alias = new.thing_alias);
        new.thingtype_id =
            (select thing.thingtype_id from thing where thing.id = new.thing_id);
        new.thingtype_alias =
            (select thing.thingtype_alias from thing where thing.id = new.thing_id);
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1thingwish_util_fields_on_update on thingwish ;
create trigger t1thingwish_util_fields_on_update before update
on thingwish for each row execute procedure thingwish_util_fields_on_update();




\echo :FILE ok
