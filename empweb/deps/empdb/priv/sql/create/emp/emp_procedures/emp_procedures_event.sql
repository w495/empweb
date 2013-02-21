\echo :FILE 'in'

/**
    @doc Обеспечивает совместное состояние события
**/

create or replace function event_util_fields_on_insert() returns "trigger" as $$
begin
    if (not (new.doc_id is null)) then
        new.target_id = new.doc_id;
        new.doc_head =
            (select doc.head from doc where doc.id = new.doc_id);
        new.doc_owner_id =
            (select doc.owner_id from doc where doc.id = new.doc_id);
        new.doc_owner_nick =
            (select doc.owner_nick from doc where doc.id = new.doc_id);
        new.doc_parent_id =
            (select doc.parent_id from doc where doc.id = new.doc_id);
        new.oktype_id =
            (select doc.oktype_id from doc where doc.id = new.doc_id);
        new.oktype_alias =
            (select doc.oktype_alias from doc where doc.id = new.doc_id);
        new.doctype_id =
            (select doc.doctype_id from doc where doc.id = new.doc_id);
        new.doctype_alias =
            (select doc.doctype_alias from doc where doc.id = new.doc_id);
        new.contype_id =
            (select doc.contype_id from doc where doc.id = new.doc_id);
        new.contype_alias =
            (select doc.contype_alias from doc where doc.id = new.doc_id);
        new.orig_id =
            (select doc.orig_id from doc where doc.id = new.doc_id);
        new.orig_owner_id =
            (select doc.orig_owner_id from doc where doc.id = new.doc_id);
        new.orig_owner_nick =
            (select doc.orig_owner_nick from doc where doc.id = new.doc_id);
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
    if (not (new.eventtype_id is null)) and (new.eventtype_alias is null) then
        new.eventtype_alias =
            (select eventtype.alias
                from
                    eventtype
                where
                    eventtype.id = new.eventtype_id);
        new.isnews =
            (select eventtype.isnews
                from
                    eventtype
                where
                    eventtype.id = new.eventtype_id);
    end if;
    if (new.eventtype_id is null) and (not (new.eventtype_alias is null)) then
        new.eventtype_id =
            (select eventtype.id
                from
                    eventtype
                where
                    eventtype.alias = new.eventtype_alias);
        new.isnews =
            (select eventtype.isnews
                from
                    eventtype
                where
                    eventtype.alias = new.eventtype_alias);
    end if;
    if (not (new.eventobj_id is null)) and (new.eventobj_alias is null) then
        new.eventobj_alias =
            (select eventobj.alias
                from
                    eventobj
                where
                    eventobj.id = new.eventobj_id);
    end if;
    if (new.eventobj_id is null) and (not (new.eventobj_alias is null)) then
        new.eventobj_id =
            (select eventobj.id
                from
                    eventobj
                where
                    eventobj.alias = new.eventobj_alias);
    end if;
    if (not (new.eventact_id is null)) and (new.eventact_alias is null) then
        new.eventact_alias =
            (select eventact.alias
                from
                    eventact
                where
                    eventact.id = new.eventact_id);
    end if;
    if (new.eventact_id is null) and (not (new.eventact_alias is null)) then
        new.eventact_id =
            (select eventact.id
                from
                    eventact
                where
                    eventact.alias = new.eventact_alias);
    end if;
    if (not (new.eventspc_id is null)) and (new.eventspc_alias is null) then
        new.eventspc_alias =
            (select eventspc.alias
                from
                    eventspc
                where
                    eventspc.id = new.eventspc_id);
    end if;
    if (new.eventspc_id is null) and (not (new.eventspc_alias is null)) then
        new.eventspc_id =
            (select eventspc.id
                from
                    eventspc
                where
                    eventspc.alias = new.eventspc_alias);
    end if;
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
    if (new.friendtype_alias is null) then
        if not (new.friendtype_id is null) then
            new.target_id = new.friendtype_id;
            new.friendtype_alias =
                (select friendtype.alias
                    from friendtype
                        where friendtype.id = new.friendtype_id);
        end if;
    end if;
    if (new.friendtype_id is null) then
        new.target_id = new.friendtype_id;
        new.friendtype_id           =
            (select friendtype.id
                from friendtype
                    where friendtype.alias = new.friendtype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1event_util_fields_on_insert on event ;
create trigger t1event_util_fields_on_insert before insert
on event for each row execute procedure event_util_fields_on_insert();


\echo :FILE ok
