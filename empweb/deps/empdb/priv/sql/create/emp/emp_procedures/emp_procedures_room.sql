\echo :FILE 'in'




/**
    @room Обеспечивает совместное состояние комнаты
**/
create or replace function room_util_fields_on_update() returns "trigger" as $$
begin

    if new.exper != old.exper then
        /**
            Авторитет комнаты,
            Перевычисляем каждый раз
            на основе его опыта.
        **/
        new.authority_alias =
            (   select
                    alias
                from
                    authority
                where
                    level
                in (    select
                            max(level)
                        from
                            authority
                        where
                            level <= new.exper
                )
            );
        /**
            Сколько еще нужно опыта
            для перехода на следующий уровень.
        **/
        new.experlack    =
            (   select
                    min(cur.level)
                from
                    authority as cur
                join
                    authority as prev
                on
                    prev.level < cur.level
                where
                    prev.alias = new.authority_alias
            ) - new.exper;
    end if;

    if not (new.experlack is null) then
        new.experlackprice  = 0.5 * new.experlack;
    else
        new.experlackprice  = null;
    end if;

    /**
        Авторитет комнаты
    **/
    if new.authority_id != old.authority_id then
        new.authority_alias =
            (select authority.alias
                from
                    authority
                where
                    authority.id = new.authority_id);
    end if;
    if new.authority_alias != old.authority_alias then
        new.authority_id =
            (select authority.id
                from
                    authority
                where
                    authority.alias = new.authority_alias);
    end if;
    
    
    /**
    *  Типы чат-комнат. (страна, тюрьма, ад, рай)
    **/
    if new.roomtype_id != old.roomtype_id then
        new.roomtype_alias =
            (select roomtype.alias
                from
                    roomtype
                where
                    roomtype.id = new.roomtype_id);
    end if;
    if new.roomtype_alias != old.roomtype_alias then
        new.roomtype_id =
            (select roomtype.id
                from
                    roomtype
                where
                    roomtype.alias = new.roomtype_alias);
    end if;
    /**
        Язык комнаты
    **/
    if new.chatlang_id != old.chatlang_id then
        new.chatlang_alias =
            (select chatlang.alias
                from
                    chatlang
                where
                    chatlang.id = new.chatlang_id);
    end if;
    if new.chatlang_alias != old.chatlang_alias then
        new.chatlang_id =
            (select chatlang.id
                from
                    chatlang
                where
                    chatlang.alias = new.chatlang_alias);
    end if;
    /**
        Режим комнаты
    **/
    if new.regimen_id != old.regimen_id then
        new.regimen_alias =
            (select regimen.alias
                from
                    regimen
                where
                    regimen.id = new.regimen_id
            );
    end if;
    if new.regimen_alias != old.regimen_alias then
        new.regimen_id =
            (select regimen.id
                from
                    regimen
                where
                    regimen.alias = new.regimen_alias
            );
    end if;

    if new.topic_id !=  old.topic_id then
        update topic set
            nchildtargets = nchildtargets + 1
        where
            id = new.topic_id;
        update topic set
            nchildtargets = nchildtargets - 1
        where
            id = old.topic_id;
    end if;


    return new;
end;
$$ language plpgsql;


create or replace function room_util_fields_on_update_doc() returns "trigger" as $$
begin
    if new.doctype_alias = 'room' then
        if (new.isdeleted = true) and (old.isdeleted = false) then
            update topic set
                nchildtargets = nchildtargets - 1
            where
                id = (select topic_id from room where room.doc_id = new.id);
        end if;
        if (new.isdeleted = false) and (old.isdeleted = true) then
            update topic set
                nchildtargets = nchildtargets + 1
            where
                id = (select topic_id from room where room.doc_id = new.id);
        end if;
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1room_util_fields_on_update_doc on doc;
create trigger t1room_util_fields_on_update_doc before update
on doc for each row execute procedure room_util_fields_on_update_doc();


drop trigger if exists t1room_util_fields_on_update on room ;
create trigger t1room_util_fields_on_update before update
on room for each row execute procedure room_util_fields_on_update();



create or replace function room_util_fields_on_insert() returns "trigger" as $$
begin

    new.back_file_id = (select file_id from back where isdefault  = true limit 1);

    /**
        Авторитет комнаты
    **/
    new.authority_alias = 'noob';
    new.authority_id    =
        (select id from authority where alias = new.authority_alias);
    /**
        Опыт комнаты
    **/
    new.exper    =
        (select level from authority where alias = new.authority_alias);
    /**
        Сколько еще нужно опыта
        для перехода на следующий уровень.
    **/
    new.experlack    =
        (   select
                min(cur.level)
            from
                authority as cur
            join
                authority as prev
            on
                prev.level < cur.level
            where
                prev.alias = new.authority_alias
        ) - new.exper;

    if not (new.experlack is null) then
        new.experlackprice  = 0.5 * new.experlack;
    else
        new.experlackprice  = null;
    end if;
    
    /**
    *  Типы чат-комнат. (страна, тюрьма, ад, рай)
    **/
    if (not (new.roomtype_id is null)) and (new.roomtype_alias is null) then
        new.roomtype_alias =
            (select roomtype.alias
                from
                    roomtype
                where
                    roomtype.id = new.roomtype_id);
    end if;
    if (new.roomtype_id is null) and (not (new.roomtype_alias is null)) then
        new.roomtype_id =
            (select roomtype.id
                from
                    roomtype
                where
                    roomtype.alias = new.roomtype_alias);
    end if;
    /*
        -- Язык комнаты
        if (not (new.chatang_id is null)) and (new.chatang_alias is null) then
            new.chatlang_alias =
                (select chatlang.alias
                    from
                        chatlang
                    where
                        chatlang.id = new.chatlang_id);
        end if;
        if (new.chatang_id is null) and (not (new.chatang_alias is null)) then
            new.chatlang_id =
                (select chatlang.id
                    from
                        chatlang
                    where
                        chatlang.alias = new.chatlang_alias);
        end if;

        -- Режим комнаты
        if (not (new.regimen_id is null)) and (new.regimen_alias is null) then
            new.regimen_alias =
                (select regimen.alias
                    from
                        regimen
                    where
                        regimen.id = new.regimen_id
                );
        end if;
        if (new.regimen_id is null) and (not (new.regimen_alias is null)) then
            new.regimen_id =
                (select regimen.id
                    from
                        regimen
                    where
                        regimen.alias = new.regimen_alias
                );
        end if;
    */
    if not (new.topic_id is null) then
        update topic set
            nchildtargets = nchildtargets + 1
        where
            id = new.topic_id;
    end if;

    return new;
end;
$$ language plpgsql;


drop trigger if exists t1room_util_fields_on_insert on room ;
create trigger t1room_util_fields_on_insert before insert
on room for each row execute procedure room_util_fields_on_insert();


create or replace function room_util_fields_on_delete() returns "trigger" as $$
begin
    if not (old.topic_id is null) then
        update topic set
            nchildtargets   = nchildtargets - 1
        where
            id = old.topic_id;
    end if;
    return old;
end;
$$ language plpgsql;

create or replace function room_util_fields_on_delete_doc() returns "trigger" as $$
begin
    if new.doctype_alias = 'room' then
        if not (old.topic_id is null) then
        update topic set
            nchildtargets   = nchildtargets - 1
        where
            id = old.topic_id;
        end if;
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1room_util_fields_on_delete_doc on doc;
create trigger t1room_util_fields_on_delete_doc before delete
on doc for each row execute procedure room_util_fields_on_delete_doc();

drop trigger if exists t1room_util_fields_on_delete on room ;
create trigger t1room_util_fields_on_delete before delete
on room for each row execute procedure room_util_fields_on_delete();



\echo :FILE ok
