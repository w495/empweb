\echo :FILE 'in'

/**
    Тригер присвоения типа документа при создании блога
**/
create or replace function on_insert_subdoc_inst() returns "trigger"
    as $$
declare
    _doc_parent_id numeric;
begin
    update doc set 
        doctype_id      = (select id from doctype where alias=TG_ARGV[0]),
        doctype_alias   = TG_ARGV[0]
    where 
        id=new.doc_id 
    returning 
        doc.parent_id 
    into 
        _doc_parent_id;
    /*
    if 'post' = TG_ARGV[0] then
        update blog set
            nposts = 1 + nposts 
        where 
            blog.doc_id = _doc_parent_id;
    end if;
    
    if 'comment' = TG_ARGV[0] then
        update post set
            ncomments = 1 + ncomments
        where 
            post.doc_id = _doc_parent_id;
    end if;
    */
    return new;
end;
$$ language plpgsql;

/**
    Тригер присвоения типа документа при создании блога
**/
drop trigger if exists t1on_insert_subdoc_inst on blog;
create trigger t1on_insert_subdoc_inst after insert
   on blog for each row execute procedure on_insert_subdoc_inst('blog');

/**
    Тригер присвоения типа документа при создании записи в блог
**/
drop trigger if exists t1on_insert_subdoc_inst on post;
create trigger t1on_insert_subdoc_inst after insert
   on post for each row execute procedure on_insert_subdoc_inst('post');

/**
    Тригер присвоения типа документа при создании комментарий записи блога
**/
drop trigger if exists t1on_insert_subdoc_inst on comment;
create trigger t1on_insert_subdoc_inst after insert
   on comment for each row execute procedure on_insert_subdoc_inst('comment');

/**
    Тригер присвоения типа документа при создании вложения
**/
drop trigger if exists t1on_insert_subdoc_inst on attach;
create trigger t1on_insert_subdoc_inst after insert
   on attach for each row execute procedure on_insert_subdoc_inst('attach');

/**
    Тригер присвоения типа документа при создании комнаты
**/
drop trigger if exists t1on_insert_subdoc_inst on room;
create trigger t1on_insert_subdoc_inst after insert
   on room for each row execute procedure on_insert_subdoc_inst('room');

/**
    Тригер присвоения типа документа при создании сообщества
**/
drop trigger if exists t1on_insert_subdoc_inst on community;
create trigger t1on_insert_subdoc_inst after insert
   on community for each row execute procedure on_insert_subdoc_inst('community');

/**
    Тригер присвоения типа документа при создании комнаты
**/
drop trigger if exists t1on_insert_subdoc_inst on message;
create trigger t1on_insert_subdoc_inst after insert
   on message for each row execute procedure on_insert_subdoc_inst('message');

/**
    Тригер присвоения типа документа при создании сообщения
**/
drop trigger if exists t1on_insert_subdoc_inst on event;
create trigger t1on_insert_subdoc_inst after insert
   on event for each row execute procedure on_insert_subdoc_inst('event');

/**
    Тригер присвоения типа документа при создании лота
**/
drop trigger if exists t1on_insert_subdoc_inst on roomlot;
create trigger t1on_insert_subdoc_inst after insert
   on roomlot for each row execute procedure on_insert_subdoc_inst('roomlot');


/**
    Тригер присвоения типа документа при создании альбома
**/
drop trigger if exists t1on_insert_subdoc_inst on album;
create trigger t1on_insert_subdoc_inst after insert
   on album for each row execute procedure on_insert_subdoc_inst('album');

/**
    Тригер присвоения типа документа при создании фотографии
**/
drop trigger if exists t1on_insert_subdoc_inst on photo;
create trigger t1on_insert_subdoc_inst after insert
   on photo for each row execute procedure on_insert_subdoc_inst('photo');

/**
    Тригер присвоения типа документа при создании напоминания пользователя
**/
drop trigger if exists t1on_insert_subdoc_inst on notice;
create trigger t1on_insert_subdoc_inst after insert
   on notice for each row execute procedure on_insert_subdoc_inst('notice');



\echo :FILE ok
