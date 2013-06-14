\echo :FILE 'in'



/**
    @doc Обеспечивает совместное состояние ставки аукциона
**/
create or replace function communitybet_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Аукцион и предмет аукциона
    **/
    if not (new.communitylot_id is null) then
        new.community_id =
            (select community_id from communitylot where communitylot.doc_id = new.communitylot_id);
        new.community_head =
            (select community_head from communitylot where communitylot.doc_id = new.communitylot_id);
    end if;
    /**
        Владелец заявки
    **/
    if (new.owner_nick is null) then
        if not (new.owner_id is null) then
            new.owner_nick =
                (select pers.nick from pers where pers.id = new.owner_id);
        end if;
    end if;
    if (new.owner_id is null) then
        new.owner_id           =
            (select pers.id from pers where pers.nick = new.owner_nick);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1communitybet_util_fields_on_insert on communitybet ;
create trigger t1communitybet_util_fields_on_insert before insert
on communitybet for each row execute procedure communitybet_util_fields_on_insert();


create or replace function communitybet_util_fields_after_insert() returns "trigger" as $$
begin
    if not (new.community_id is null) then
        update community set
            communitybet_id          = new.id,
            communitybet_price       = new.price,
            communitybet_owner_id    = new.owner_id,
            communitybet_owner_nick  = new.owner_nick
        where
            community.doc_id = new.community_id;
    end if;
    if not (new.communitylot_id is null) then
        update communitylot set
            betcur              = new.price
        where
            communitylot.doc_id = new.communitylot_id;
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1communitybet_util_fields_after_insert on communitybet ;
create trigger t1communitybet_util_fields_after_insert after insert
on communitybet for each row execute procedure communitybet_util_fields_after_insert();



create or replace function communitybet_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Аукцион и предмет аукциона
    **/
    if new.communitylot_id != old.communitylot_id then
        new.community_id =
            (select community_id from communitylot where communitylot.doc_id = new.communitylot_id);
        new.community_head =
            (select community_head from communitylot where communitylot.doc_id = new.communitylot_id);
    end if;
    /**
        Владелец заявки
    **/
    if new.owner_id != old.owner_id then
        new.owner_nick =
            (select pers.nick from pers where pers.id = new.owner_id);
        update community set
            communitybet_owner_nick  = new.owner_nick,
            communitybet_owner_id    = new.owner_id
        where
            community.doc_id = new.community_id;
    end if;
    /** ------------------------------------------ **/
    if new.owner_nick != old.owner_nick then
        new.owner_id =
            (select pers.id from pers where pers.nick = new.owner_nick);
        update community set
            communitybet_owner_nick  = new.owner_nick,
            communitybet_owner_id    = new.owner_id
        where
            community.doc_id = new.community_id;
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1communitybet_util_fields_on_update on communitybet ;
create trigger t1communitybet_util_fields_on_update before update
on communitybet for each row execute procedure communitybet_util_fields_on_update();




\echo :FILE ok
