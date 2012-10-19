

/**
    @doc 
**/
create or replace function count_posts_on_insert() returns "trigger" as $$
declare
    new_parent_id numeric;
    new_read_acctype_alias varchar(1024);
begin
    select parent_id, read_acctype_alias from doc 
        join post on
            post.doc_id = doc.id
        where post.doc_id = new.doc_id into new_parent_id, new_read_acctype_alias;
    /** Если родитяля нет, то делать ничего не нужно **/
    if not (new_parent_id is null) then
        update blog set nposts = 1 + nposts where blog.doc_id = new_parent_id;
        if new_read_acctype_alias = 'private' then
            update blog set nprivateposts = 1 + nprivateposts where blog.doc_id = new_parent_id;
        end if;
        elsif new_read_acctype_alias = 'public' then
            update blog set npublicposts = 1 + npublicposts where blog.doc_id = new_parent_id;
        elsif new_read_acctype_alias = 'protected' then
            update blog set nprotectedposts = 1 + nprotectedposts where blog.doc_id = new_parent_id;
        end if;
    end if;
    return new;
end;
$$ language plpgsql;


create or replace function count_posts_on_update() returns "trigger" as $$
declare
    new_parent_id numeric;
    new_isdeleted boolean;
    new_read_acctype_alias varchar(1024);
    old_parent_id numeric;
    old_isdeleted boolean;
    old_read_acctype_alias varchar(1024);
begin
    select parent_id, read_acctype_alias, isdeleted from doc 
        join post on
            post.doc_id = doc.id
        where post.doc_id = old.doc_id into old_parent_id, old_read_acctype_alias, old_isdeleted;
    select parent_id, read_acctype_alias, isdeleted  from doc 
        join post on
            post.doc_id = doc.id
        where post.doc_id = new.doc_id into new_parent_id, new_read_acctype_alias, new_isdeleted;
        
    /** Если родитяля нет, то делать ничего не нужно **/
    if not (new_parent_id is null) then    
        /** смена типа **/
        if (old_read_acctype_alias != new_read_acctype_alias) then
            if new_read_acctype_alias = 'private' then
                update blog set nprivateposts = 1 + nprivateposts where blog.doc_id = new_parent_id;
            end if;
            elsif new_read_acctype_alias = 'public' then
                update blog set npublicposts = 1 + npublicposts where blog.doc_id = new_parent_id;
            elsif new_sread_acctype_alias = 'protected' then
                update blog set nprotectedposts = 1 + nprotectedposts where blog.doc_id = new_parent_id;
            end if;
        end if;
        /** смена родителя **/
        if (new_parent_id != old_parent_id) then
            update blog set nposts = nposts + 1 where blog.doc_id = new_parent_id;
            update blog set nposts = nposts - 1 where blog.doc_id = old_parent_id;
            if new.read_acctype_alias = 'private' then
                update blog set nprivateposts = nprivateposts + 1  where blog.doc_id = new_parent_id;
                update blog set nprivateposts = nprivateposts - 1  where blog.doc_id = old_parent_id;
            end if;
            elsif new.read_acctype_alias = 'public' then
                update blog set npublicposts = npublicposts  + 1 where blog.doc_id = new_parent_id;
                update blog set npublicposts = npublicposts  - 1 where blog.doc_id = old_parent_id;
            elsif new.read_acctype_alias = 'protected' then
                update blog set nprotectedposts = nprotectedposts  + 1 where blog.doc_id = new_parent_id;
                update blog set nprotectedposts = nprotectedposts  - 1 where blog.doc_id = old_parent_id;
            end if;
        end if;
        if (old_isdeleted = 'false' and new_isdeleted = 'true') then
            update blog set nposts = nposts - 1 where blog.doc_id = old_parent_id;
            if old_read_acctype_alias = 'private' then
                update blog set nprivateposts = nprivateposts - 1 where blog.doc_id = old_parent_id;
            end if;
            elsif old_read_acctype_alias = 'public' then
                update blog set npublicposts = npublicposts - 1 where blog.doc_id = old_parent_id;
            elsif old_read_acctype_alias = 'protected' then
                update blog set nprotectedposts = nprotectedposts - 1 where blog.doc_id = old_parent_id;
            end if;
        end if;
        /** смена родителя **/
        
    end if;
    return new;
end;
$$ language plpgsql;



create or replace function count_posts_on_delete() returns "trigger" as $$
declare
    old_parent_id numeric;
begin
    select parent_id from doc 
        join post on
            post.doc_id = doc.id
        where post.doc_id = old.doc_id into old_parent_id;
    /** Если родитяля нет, то делать ничего не нужно **/
    if not (old_parent_id is null) then
        update blog set nposts = nposts - 1 where blog.doc_id = old_parent_id;
        if old_read_acctype_alias = 'private' then
            update blog set nprivateposts = nprivateposts - 1 where blog.doc_id = old_parent_id;
        end if;
        elsif old_read_acctype_alias = 'public' then
            update blog set npublicposts = npublicposts - 1 where blog.doc_id = old_parent_id;
        elsif old_read_acctype_alias = 'protected' then
            update blog set nprotectedposts = nprotectedposts - 1 where blog.doc_id = old_parent_id;
        end if;
    end if;
    return old;
end;
$$ language plpgsql;

