\echo :FILE 'in'



/**
    @doc Возвращает начальное состояние документа
**/
create or replace function tr_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Псевдоним языка \ Id языка
    **/
    if (new.lang_alias is null) then
        if not (new.lang_id is null) then
            new.lang_alias =
                (select alias from lang where id = new.lang_id);
        else
            new.lang_alias        = 'en_gb';
        end if;
    end if;
    if (new.lang_id is null) then
        new.lang_id           =
            (select id from lang    where alias = new.lang_alias);
    end if;
    /**
        Тип перевода: статичный, динамичный
    **/
    if (new.trtype_alias is null) then
        if not (new.trtype_id is null) then
            new.trtype_alias =
                (select alias from trtype where id = new.trtype_id);
        else
            new.trtype_alias        = 'dynamic';
        end if;
    end if;
    if (new.trtype_id is null) then
        new.trtype_id           =
            (select id from trtype    where alias = new.trtype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1tr_util_fields_on_insert on tr ;
create trigger t1tr_util_fields_on_insert before insert
on tr for each row execute procedure tr_util_fields_on_insert();

/**
    @doc Обеспечивает совместное состояние документа
**/
create or replace function tr_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Псевдоним языка \ Id языка
    **/
    if new.lang_id != old.lang_id then
        new.lang_alias =
            (select lang.alias from lang where lang.id = new.lang_id);
    end if;
    if new.lang_alias != old.lang_alias then
        new.lang_id =
            (select lang.id from lang where lang.alias = new.lang_alias);
    end if;
    /**
        Тип перевода: статичный, динамичный
    **/
    if new.trtype_id != old.trtype_id then
        new.trtype_alias =
            (select trtype.alias
                from
                    trtype
                where
                    trtype.id = new.trtype_id);
    end if;
    if new.trtype_alias != old.trtype_alias then
        new.trtype_id =
            (select trtype.id
                from
                    trtype
                where
                    trtype.alias = new.trtype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1tr_util_fields_on_update on tr ;
create trigger t1tr_util_fields_on_update before update
on tr for each row execute procedure tr_util_fields_on_update();





\echo :FILE ok
