\echo :FILE 'in'




/**
    @doc Возвращает начальное состояние документа
**/
create or replace function doc_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Владелец документа
    **/
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

    /**
        Владелец оригинального документа
    **/
    if (new.orig_owner_nick is null) then
        if not (new.orig_owner_id is null) then
            new.orig_owner_nick =
                (select pers.nick from pers where pers.id = new.orig_owner_id);
        else
            new.orig_owner_nick        = null;
        end if;
    end if;
    if (new.orig_owner_id is null) then
        new.orig_owner_id           =
            (select pers.id from pers where pers.nick = new.orig_owner_nick);
    end if;

    /**
        Непросмотрен, разрешен, запрещен, там где это нужно,
    **/
    if (new.oktype_alias is null) then
        if not (new.oktype_id is null) then
            new.oktype_alias =
                (select alias from oktype where id = new.oktype_id);
        else
            new.oktype_alias        = 'ncons';
        end if;
    end if;
    if (new.oktype_id is null) then
        new.oktype_id           =
            (select id from oktype    where alias = new.oktype_alias);
    end if;
    /**
        Разрешение на чтение
    **/
    if (new.read_acctype_alias is null) then
        if not (new.read_acctype_id is null) then
            new.read_acctype_alias =
                (select alias from acctype where id = new.read_acctype_id);
        else
            new.read_acctype_alias  = 'public';
        end if;
    end if;
    if (new.read_acctype_id is null) then
        new.read_acctype_id     =
            (select id from acctype   where alias = new.read_acctype_alias);
    end if;
    /**
        Разрешение комментов
    **/
    if (new.comm_acctype_alias is null) then
        if not (new.comm_acctype_id is null) then
            new.comm_acctype_alias =
                (select alias from acctype where id = new.comm_acctype_id);
        else
            new.comm_acctype_alias  = 'private';
        end if;
    end if;
    if (new.comm_acctype_id is null) then
        new.comm_acctype_id     =
            (select id from acctype   where alias = new.comm_acctype_alias);
    end if;
    /**
        Типы контента: Обычный, эротический
    **/
    if (new.contype_alias is null) then
        if not (new.contype_id is null) then
            new.contype_alias =
                (select alias from contype where id = new.contype_id);
        else
            new.contype_alias       = 'common';
        end if;
    end if;
    if (new.contype_id is null) then
        new.contype_id          =
            (select id from contype   where alias = new.contype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1doc_util_fields_on_insert on doc ;
create trigger t1doc_util_fields_on_insert before insert
on doc for each row execute procedure doc_util_fields_on_insert();

/**
    @doc Обеспечивает совместное состояние документа
**/
create or replace function doc_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Язык документа
    **/
    if new.lang_id != old.lang_id then
        new.lang_alias =
            (select lang.alias
                from
                    lang
                where
                    lang.id = new.lang_id);
    end if;
    if new.lang_alias != old.lang_alias then
        new.lang_id =
            (select lang.id
                from
                    lang
                where
                    lang.alias = new.lang_alias);
    end if;
    /**
        Владелец документа
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
        Владелец оригинального документа
    **/
    if new.orig_owner_id != old.orig_owner_id then
        new.orig_owner_nick =
            (select pers.nick from pers where pers.id = new.orig_owner_id);
    end if;
    if new.orig_owner_nick != old.orig_owner_nick then
        new.orig_owner_id =
            (select pers.id from pers where pers.nick = new.orig_owner_nick);
    end if;


    if new.head != old.head then
        if new.doctype_alias = 'room' then
            update pers set live_room_head = new.head
                where pers.live_room_id = new.id;
            update pers set own_room_head = new.head
                where pers.own_room_id = new.id;
        end if;
        if new.doctype_alias = 'community' then
            update pers set community_head = new.head
                where pers.community_id = new.id;
        end if;
    end if;
    /**
        Непросмотрен, разрешен, запрещен, там где это нужно,
    **/
    if new.oktype_id != old.oktype_id then
        new.oktype_alias =
            (select oktype.alias
                from oktype
                    where oktype.id = new.oktype_id);
    end if;
    if new.oktype_alias != old.oktype_alias then
        new.oktype_id =
            (select oktype.id
                from oktype
                    where oktype.alias = new.oktype_alias);
    end if;
    /**
        Тип документа: блог, коммент к блогу, галерея,
            фото, коммент к фото, attach descr.
    **/
    if new.doctype_id != old.doctype_id then
        new.doctype_alias =
            (select doctype.alias
                from
                    doctype
                where
                    doctype.id = new.doctype_id);
    end if;
    if new.doctype_alias != old.doctype_alias then
        new.doctype_id =
            (select doctype.id
                from
                    doctype
                where
                    doctype.alias = new.doctype_alias);
    end if;
    /**
        Разрешение на чтение
    **/
    if new.read_acctype_id != old.read_acctype_id then
        new.read_acctype_alias =
            (select acctype.alias
                from
                    acctype
                where
                    acctype.id = new.read_acctype_id);
    end if;
    if new.read_acctype_alias != old.read_acctype_alias then
        new.read_acctype_id =
            (select acctype.id
                from
                    acctype
                where
                    acctype.alias = new.read_acctype_alias);
    end if;
    /**
        Разрешение комментов
    **/
    if new.comm_acctype_id != old.comm_acctype_id then
        new.comm_acctype_alias =
            (select acctype.alias
                from
                    acctype
                where
                    acctype.id = new.comm_acctype_id
            );
    end if;
    if new.comm_acctype_alias != old.comm_acctype_alias then
        new.comm_acctype_id =
            (select acctype.id
                from
                    acctype
                where
                    acctype.alias = new.comm_acctype_alias
            );
    end if;
    /**
        Типы контента: Обычный, эротический
    **/
    if new.contype_id != old.contype_id then
        new.contype_alias =
            (select contype.alias
                from
                    contype
                where
                    contype.id = new.contype_id);
    end if;
    if new.contype_alias != old.contype_alias then
        new.contype_id =
            (select contype.id
                from
                    contype
                where
                    contype.alias = new.contype_alias);
    end if;

    /**
        Делаем репост зависимым по дереву.
        Если ставим isrepostable = false
        для родителя, то тоже самое ставится для его детей.
    **/
    if new.isrepostable != old.isrepostable then
        update doc set isrepostable = new.isrepostable
            where parent_id = new.id and owner_id = new.owner_id;
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1doc_util_fields_on_update on doc ;
create trigger t1doc_util_fields_on_update before update
on doc for each row execute procedure doc_util_fields_on_update();



\echo :FILE ok
