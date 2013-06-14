\echo :FILE 'in'




--(2012.10.22 16:01:55:586728391)---------------------------------------------


/**
    @doc Обеспечивает совместное состояние лота аукциона
**/
create or replace function cdoclot_util_fields_on_insert() returns "trigger" as $$
begin
    if not (new.cdoc_id is null) then
        new.cdoc_head =
            (select head from doc where doc.id = new.cdoc_id);
    end if;
    new.betcur = new.betmin;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1cdoclot_util_fields_on_insert on cdoclot ;
create trigger t1cdoclot_util_fields_on_insert before insert
on cdoclot for each row execute procedure cdoclot_util_fields_on_insert();


create or replace function cdoclot_util_fields_after_insert() returns "trigger" as $$
begin
    if not (new.cdoc_id is null) then
        update doc set
            cdoclot_id      = new.doc_id,
            cdoclot_betmin  = new.betmin,
            cdoclot_betmax  = new.betmax,
            cdoclot_dtstart = new.dtstart,
            cdoclot_dtstop  = new.dtstop
        where
            doc.id = new.cdoc_id;
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1cdoclot_util_fields_after_insert on cdoclot ;
create trigger t1cdoclot_util_fields_after_insert after insert
on cdoclot for each row execute procedure cdoclot_util_fields_after_insert();

create or replace function cdoclot_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Плательщик покупки
    **/
    if (new.cdoc_id != old.cdoc_id) then
        new.cdoc_head =
            (select head from doc where doc.id = new.cdoc_id);
        if (not (new.cdoc_id is null)) then
            update cdoc set
                cdoclot_id      = null,
                cdoclot_betmin  = null,
                cdoclot_betmax  = null,
                cdoclot_dtstart = null,
                cdoclot_dtstop  = null
            where
                doc.id = old.cdoc_id;
            update cdoc set
                cdoclot_id      = new.doc_id,
                cdoclot_betmin  = new.betmin,
                cdoclot_betmax  = new.betmax,
                cdoclot_dtstart = new.dtstart,
                cdoclot_dtstop  = new.dtstop
            where
                doc.id = new.cdoc_id;
        end if;
    end if;

    if (new.betmin != old.betmin) and (not (new.cdoc_id is null)) then
        update doc set
            cdoclot_betmin  = new.betmin
        where
            doc.id = new.cdoc_id;
    end if;

    if (new.betmax != old.betmax) and (not (new.cdoc_id is null)) then
        update doc set
            cdoclot_betmax  = new.betmax
        where
            doc.id = new.cdoc_id;
    end if;

    if (new.dtstart != old.dtstart) and (not (new.cdoc_id is null)) then
        update doc set
            cdoclot_dtstart  = new.dtstart
        where
            doc.id = new.cdoc_id;
    end if;

    if (new.dtstop != old.dtstop) and (not (new.cdoc_id is null)) then
        update doc set
            cdoclot_dtstop  = new.dtstop
        where
            doc.id = new.cdoc_id;
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1cdoclot_util_fields_on_update on cdoclot ;
create trigger t1cdoclot_util_fields_on_update before update
on cdoclot for each row execute procedure cdoclot_util_fields_on_update();



\echo :FILE ok
