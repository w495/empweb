\echo :FILE 'in'




--(2012.10.22 16:01:55:586728391)---------------------------------------------


/**
    @doc Обеспечивает совместное состояние лота аукциона
**/
create or replace function communitylot_util_fields_on_insert() returns "trigger" as $$
begin
    if not (new.community_id is null) then
        new.community_head =
            (select head from doc where doc.id = new.community_id);
    end if;
    new.betcur = new.betmin;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1communitylot_util_fields_on_insert on communitylot ;
create trigger t1communitylot_util_fields_on_insert before insert
on communitylot for each row execute procedure communitylot_util_fields_on_insert();


create or replace function communitylot_util_fields_after_insert() returns "trigger" as $$
begin
    if not (new.community_id is null) then
        update community set
            communitylot_id      = new.doc_id,
            communitylot_betmin  = new.betmin,
            communitylot_betmax  = new.betmax,
            communitylot_dtstart = new.dtstart,
            communitylot_dtstop  = new.dtstop
        where
            community.doc_id = new.community_id;
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1communitylot_util_fields_after_insert on communitylot ;
create trigger t1communitylot_util_fields_after_insert after insert
on communitylot for each row execute procedure communitylot_util_fields_after_insert();

create or replace function communitylot_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Плательщик покупки
    **/
    if (new.community_id != old.community_id) then
        new.community_head =
            (select head from doc where doc.id = new.community_id);
        if (not (new.community_id is null)) then
            update community set
                communitylot_id      = null,
                communitylot_betmin  = null,
                communitylot_betmax  = null,
                communitylot_dtstart = null,
                communitylot_dtstop  = null
            where
                community.doc_id = old.community_id;
            update community set
                communitylot_id      = new.doc_id,
                communitylot_betmin  = new.betmin,
                communitylot_betmax  = new.betmax,
                communitylot_dtstart = new.dtstart,
                communitylot_dtstop  = new.dtstop
            where
                community.doc_id = new.community_id;
        end if;
    end if;

    if (new.betmin != old.betmin) and (not (new.community_id is null)) then
        update community set
            communitylot_betmin  = new.betmin
        where
            community.doc_id = new.community_id;
    end if;

    if (new.betmax != old.betmax) and (not (new.community_id is null)) then
        update community set
            communitylot_betmax  = new.betmax
        where
            community.doc_id = new.community_id;
    end if;

    if (new.dtstart != old.dtstart) and (not (new.community_id is null)) then
        update community set
            communitylot_dtstart  = new.dtstart
        where
            community.doc_id = new.community_id;
    end if;

    if (new.dtstop != old.dtstop) and (not (new.community_id is null)) then
        update community set
            communitylot_dtstop  = new.dtstop
        where
            community.doc_id = new.community_id;
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1communitylot_util_fields_on_update on communitylot ;
create trigger t1communitylot_util_fields_on_update before update
on communitylot for each row execute procedure communitylot_util_fields_on_update();



\echo :FILE ok
