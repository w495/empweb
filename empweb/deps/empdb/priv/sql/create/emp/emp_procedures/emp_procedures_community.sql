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
    if new.communitytype_id is distinct from  old.communitytype_id then
        new.communitytype_alias =
            (select communitytype.alias
                from
                    communitytype
                where
                    communitytype.id = new.communitytype_id);
    end if;
    if new.communitytype_alias is distinct from  old.communitytype_alias then
        new.communitytype_id =
            (select communitytype.id
                from
                    communitytype
                where
                    communitytype.alias = new.communitytype_alias);
    end if;

    if new.cands_gte_authority_id is distinct from  old.cands_gte_authority_id then
        new.cands_gte_authority_alias =
            (select authority.alias
                from
                    authority
                where
                    authority.id = new.cands_gte_authority_id);
        new.cands_gte_authority_level =
            (select authority.level
                from
                    authority
                where
                    authority.id = new.cands_gte_authority_id);
    end if;
    if new.cands_gte_authority_alias is distinct from  old.cands_gte_authority_alias then
        new.cands_gte_authority_id =
            (select authority.id
                from
                    authority
                where
                    authority.alias = new.cands_gte_authority_alias);
        new.cands_gte_authority_level =
            (select authority.level
                from
                    authority
                where
                    authority.alias = new.cands_gte_authority_alias);
    end if;

    
    if new.read_gte_authority_id is distinct from  old.read_gte_authority_id then
        new.read_gte_authority_alias =
            (select authority.alias
                from
                    authority
                where
                    authority.id = new.read_gte_authority_id);
        new.read_gte_authority_level =
            (select authority.level
                from
                    authority
                where
                    authority.id = new.read_gte_authority_id);
    end if;
    if new.read_gte_authority_alias is distinct from  old.read_gte_authority_alias then
        new.read_gte_authority_id =
            (select authority.id
                from
                    authority
                where
                    authority.alias = new.read_gte_authority_alias);
        new.read_gte_authority_level =
            (select authority.level
                from
                    authority
                where
                    authority.alias = new.read_gte_authority_alias);
    end if;
    
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1community_util_fields_on_update on community ;
create trigger t1community_util_fields_on_update before update
on community for each row execute procedure community_util_fields_on_update();


create or replace function community_util_fields_on_insert() returns "trigger" as $$
begin
    new.back_file_id = (select file_id from back where isdefault  = true limit 1);

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

    if (new.cands_gte_authority_id is not  null) and (new.cands_gte_authority_alias is not  null) then
        new.read_gte_authority_alias = 'noob';
    end if;
    
    if (not (new.cands_gte_authority_id is null))
        and (new.cands_gte_authority_alias is null) then
        new.cands_gte_authority_alias =
            (select authority.alias
                from
                    authority
                where
                    authority.id = new.cands_gte_authority_id);
        new.cands_gte_authority_level =
            (select authority.level
                from
                    authority
                where
                    authority.id = new.cands_gte_authority_id);
    end if;
    if (new.cands_gte_authority_id is null)
        and (not (new.cands_gte_authority_alias is null)) then
        new.cands_gte_authority_id =
            (select authority.id
                from
                    authority
                where
                    authority.alias = new.cands_gte_authority_alias);
        new.cands_gte_authority_level =
            (select authority.level
                from
                    authority
                where
                    authority.alias = new.cands_gte_authority_alias);
    end if;



    if (new.read_gte_authority_id is not  null) and (new.read_gte_authority_alias is not  null) then
        new.read_gte_authority_alias = 'noob';
    end if;
    
    if (not (new.read_gte_authority_id is null))
        and (new.read_gte_authority_alias is null) then
        new.read_gte_authority_alias =
            (select authority.alias
                from
                    authority
                where
                    authority.id = new.read_gte_authority_id);
        new.read_gte_authority_level =
            (select authority.level
                from
                    authority
                where
                    authority.id = new.read_gte_authority_id);
    end if;
    if (new.read_gte_authority_id is null)
        and (not (new.read_gte_authority_alias is null)) then
        new.read_gte_authority_id =
            (select authority.id
                from
                    authority
                where
                    authority.alias = new.read_gte_authority_alias);
        new.read_gte_authority_level =
            (select authority.level
                from
                    authority
                where
                    authority.alias = new.read_gte_authority_alias);
    end if;
    
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1community_util_fields_on_insert on community ;
create trigger t1community_util_fields_on_insert before insert
on community for each row execute procedure community_util_fields_on_insert();


\echo :FILE ok
