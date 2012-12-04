\echo :FILE 'in'




/**
    --------------------------------------------------------------------------
    @doc Обеспечивает совместное состояние cooбщества
    --------------------------------------------------------------------------
**/
create or replace function community_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Типы сообщества
    **/
    if new.communitytype_id != old.communitytype_id then
        new.communitytype_alias =
            (select communitytype.alias
                from
                    communitytype
                where
                    communitytype.id = new.communitytype_id);
    end if;
    if new.communitytype_alias != old.communitytype_alias then
        new.communitytype_id =
            (select communitytype.id
                from
                    communitytype
                where
                    communitytype.alias = new.communitytype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1community_util_fields_on_update on community ;
create trigger t1community_util_fields_on_update before update
on community for each row execute procedure community_util_fields_on_update();


create or replace function community_util_fields_on_insert() returns "trigger" as $$
begin
    /**
    *  Типы сообщества
    **/
    if (not (new.communitytype_id is null))
        and (new.communitytype_alias is null) then
        new.communitytype_alias =
            (select communitytype.alias
                from
                    communitytype
                where
                    communitytype.id = new.communitytype_id);
    end if;
    if (new.communitytype_id is null)
        and (not (new.communitytype_alias is null)) then
        new.communitytype_id =
            (select communitytype.id
                from
                    communitytype
                where
                    communitytype.alias = new.communitytype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1community_util_fields_on_insert on community ;
create trigger t1community_util_fields_on_insert before insert
on community for each row execute procedure community_util_fields_on_insert();




\echo :FILE ok
