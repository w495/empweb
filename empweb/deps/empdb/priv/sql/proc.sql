-- create or replace language plpgsql;
create language plpgsql;


   
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
drop trigger if exists on_insert_subdoc_inst on blog;
create trigger on_insert_subdoc_inst after insert
   on blog for each row execute procedure on_insert_subdoc_inst('blog');

/**
    Тригер присвоения типа документа при создании записи в блог
**/
drop trigger if exists on_insert_subdoc_inst on post;
create trigger on_insert_subdoc_inst after insert
   on post for each row execute procedure on_insert_subdoc_inst('post');

/**
    Тригер присвоения типа документа при создании комментарий записи блога
**/
drop trigger if exists on_insert_subdoc_inst on comment;
create trigger on_insert_subdoc_inst after insert
   on comment for each row execute procedure on_insert_subdoc_inst('comment');

/**
    Тригер присвоения типа документа при создании вложения
**/
drop trigger if exists on_insert_subdoc_inst on attach;
create trigger on_insert_subdoc_inst after insert
   on attach for each row execute procedure on_insert_subdoc_inst('attach');

/**
    Тригер присвоения типа документа при создании комнаты
**/
drop trigger if exists on_insert_subdoc_inst on room;
create trigger on_insert_subdoc_inst after insert
   on room for each row execute procedure on_insert_subdoc_inst('room');

/**
    Тригер присвоения типа документа при создании сообщества
**/
drop trigger if exists on_insert_subdoc_inst on community;
create trigger on_insert_subdoc_inst after insert
   on community for each row execute procedure on_insert_subdoc_inst('community');

/**
    Тригер присвоения типа документа при создании комнаты
**/
drop trigger if exists on_insert_subdoc_inst on message;
create trigger on_insert_subdoc_inst after insert
   on message for each row execute procedure on_insert_subdoc_inst('message');

/**
    Тригер присвоения типа документа при создании сообщения
**/
drop trigger if exists on_insert_subdoc_inst on event;
create trigger on_insert_subdoc_inst after insert
   on event for each row execute procedure on_insert_subdoc_inst('event');
 
/**
    Aтомарное создание комнаты для новичков.
**/

create sequence seq_noobsroom_id;
create or replace function  mknoobsroom() returns numeric as $$
declare
     _res numeric;
begin
        select mknoobsroom((select -nextval('seq_noobsroom_id'))) into _res;
        return _res;
end;
$$ language 'plpgsql';


create or replace function  mknoobsroom(did numeric) returns numeric as $$
declare
     _doc_id numeric;
begin
    lock table room  in exclusive mode;
        insert into doc (
            "id",
            "head", 
            "body"
        ) values (
            $1, 
            'head', 
            'body'
        ) returning id into _doc_id;    
    begin
        insert into room (
            doc_id, 
            roomtype_id
        ) 
        values (
            _doc_id, 
            (select "id" from roomtype where alias='noobs')
        );
    exception
         when unique_violation then
    end;
    return _doc_id as "id";
end;
$$ language 'plpgsql';


/**
    Тригер, который засовывает новичков в случайную комнату для новичков.
**/
create or replace function  noobsroom() returns numeric as $$
declare
     _res numeric;
begin
    select doc_id from room 
        join roomtype on 
            roomtype.id = room.roomtype_id 
            and roomtype.alias='noobs' 
            order by random() limit 1 into _res;
    if _res is null then
        perform mknoobsroom();
        select noobsroom() into _res;
        return _res;
    else
        return _res;
    end if;
end;
$$ language 'plpgsql';


/**
    ==========================================================================
        Тригеры для подсчета дочерних элементов иерархических сущностей.

        Формат:
            Для того, чтобы не дублировать аналогичный код для разных
            таблиц было сделано выполнение строки с подставленным именем
            таблицы. (!!!) Это может вызвать некоторые замедления.
            Имя таблицы при создании триггеров передается как строка.
            
                select count_children_on('mytable');
                
           Для того чтобы воспользоваться триггерами в отношении 
           должны быть опеределены поля:
           
                isdeleted   bool default false,
                    --- флаг отсутвия элемента;
                parent_id   decimal references <таблица>(id), 
                    --- сcылка на родителя;
                nchildren   decimal default 0, 
                    --- количество детей (прямых потомков);
                nnodes      decimal default 0, 
                    --- количество вершин в кусте.
                
        При вставке элемента, если для него указан родитель,
        число прямых потомков и всех потомков увеличивается на единицу.
        Для родитяля, у которого появился новый потомок, число потомкови
        его родителя увеличивается (см код).
        При псевдоудалении элементов (update <таблица> set isdeleted = true), 
        число прямых и всех потомков уменьшается на 1 и размер удаленного 
        поддерева соответвсенно.
        При псевдовосстановлении (update set <таблица> isdeleted = true) 
        --- увеличивается. При смене родителя размер дерева прибавляется 
        к новому родителю и отнимается от старого.
        При явном удалении (delete from <таблица> ), 
        число прямых и всех потомков уменьшается на 1 и размер удаленного 
        поддерева соответвсенно.
        
                
    ==========================================================================
**/
 
/**
    @doc Возвращает триггер обновления
**/
create or replace function count_children_on_update() returns "trigger" as $$
begin
    /** 
        Если родитяля нет, то делать ничего не нужно,
        условие остановки обхода дерева вершин.
    **/
    if not (new.parent_id is null) then
        /**
            Появление внука.
        **/
        if  new.nnodes  != old.nnodes then
            /**
                Все потомки (количество вершин в кусте).
                Здесь мы инициируем пересчет потомков у родителя текущего узла 
                и продолжаем обход дерева вершин.
            **/
            execute 'update '||quote_ident(tg_table_name) ||'
            set 
                nnodes = nnodes + '|| cast((new.nnodes - old.nnodes) as varchar) ||' 
            where 
                '||cast(new.parent_id as varchar)||
                ' = '||quote_ident(tg_table_name)||'.id;';
        end if;
        /**
            Псевдоудаление.
        **/
        if  new.isdeleted = true and old.isdeleted = false then
            /**
                Прямые потомки.
            **/
            execute 'update '||quote_ident(tg_table_name) ||' 
            set nchildren = nchildren - 1 
            where 
                '||cast(new.parent_id as varchar)||
                ' = '||quote_ident(tg_table_name)||'.id;';
            /**
                Все потомки (количество вершин в кусте).
            **/
            execute 'update '||quote_ident(tg_table_name) ||'
            set nnodes = nnodes - '||cast(new.nnodes  as varchar)||'
            where 
                '||cast(new.parent_id as varchar)||
                ' = '||quote_ident(tg_table_name)||'.id;';
        /**
            Псевдовосстановление.
        **/
        elsif new.isdeleted = false  and old.isdeleted = true then
            /**
                Прямые потомки.
            **/
            execute 'update '||quote_ident(tg_table_name) ||'
            set nchildren = nchildren + 1 
            where 
                '||cast(new.parent_id as varchar)||
                ' = '||quote_ident(tg_table_name)||'.id;';
            /**
                Все потомки (количество вершин в кусте).
            **/
            execute 'update '||quote_ident(tg_table_name) ||'
            set nnodes = nnodes + '||cast(new.nnodes  as varchar)||'
            where 
                '||cast(new.parent_id as varchar)||
                ' = '||quote_ident(tg_table_name)||'.id;';
        end if;
    end if;
    /**
        Перемещение.
        Проверку на наличие родителя надо проводить аккуратно, 
        так как текущий элемент может сам стать корнем,
        или стать частью другого дерева, если был корнем.
    **/
    if new.parent_id != old.parent_id then
        if not (old.parent_id is null) then
            /**
                Прямые потомки.
            **/
            execute 'update '||quote_ident(tg_table_name) ||'
            set nnodes = nnodes - '||cast(new.nnodes  as varchar)||'
            where 
                '||cast(old.parent_id as varchar)||
                ' = '||quote_ident(tg_table_name)||'.id;';     
            /**
                Все потомки (количество вершин в кусте).
            **/
            execute 'update '||quote_ident(tg_table_name) ||'
            set nchildren = nchildren - 1 
            where 
                '||cast(old.parent_id as varchar)||
                ' = '||quote_ident(tg_table_name)||'.id;';
        end if;
        if not (new.parent_id is null) then
            /**
                Прямые потомки.
            **/
            execute 'update '||quote_ident(tg_table_name) ||'
            set nnodes = nnodes + '||cast(new.nnodes  as varchar)||'
            where 
                '||cast(new.parent_id as varchar)||
                ' = '||quote_ident(tg_table_name)||'.id;';
            /**
                Все потомки (количество вершин в кусте).
            **/
            execute 'update '||quote_ident(tg_table_name) ||'
            set nchildren = nchildren + 1 
            where 
                '||cast(new.parent_id as varchar)||
                ' = '||quote_ident(tg_table_name)||'.id;';
        end if;
    end if;
    return new;
end;
$$ language plpgsql;

/**
    @doc Возвращает триггер вставки.
**/
create or replace function count_children_on_insert() returns "trigger" as $$
begin
    /** Если родитяля нет, то делать ничего не нужно **/
    if not (new.parent_id is null) then
        /**
            Прямые потомки.
        **/
        execute 'update '||quote_ident(tg_table_name)||'
        set nchildren = 1 + nchildren 
        where 
            '||cast(new.parent_id as varchar)||
            ' = '||quote_ident(tg_table_name)||'.id;';
        /**
            Все потомки (количество вершин в кусте).
            Здесь мы инициируем пересчет потомков у родителя текущего узла 
            и начинанем обход дерева вершин.
        **/
        execute 'update '||quote_ident(tg_table_name)||'
        set nnodes = 1 + nnodes 
        where 
            '||cast(new.parent_id as varchar)||
            ' = '||quote_ident(tg_table_name)||'.id;';
    end if;
    return new;
end;
$$ language plpgsql;

/**
    @doc Возвращает триггер удаления.
**/
create or replace function count_children_on_delete() returns "trigger" as $$
begin
    if old.parent_id != null then
        /**
            Прямые потомки.
        **/
        execute 'update '||quote_ident(tg_table_name)||'
        set 
            nchildren = nchildren - 1
        where 
            '||cast(old.parent_id as varchar)||
            ' = '||quote_ident(tg_table_name)||'.id;';
        /**
            Все потомки (количество вершин в кусте).
            Здесь мы инициируем пересчет потомков у родителя текущего узла 
            и начинанем обход дерева вершин.
        **/
        execute 'update '||quote_ident(tg_table_name)||'
        set 
            nnodes = nnodes - 1
        where 
            '||cast(old.parent_id as varchar)||
            ' = '||quote_ident(tg_table_name)||'.id;';
    end if;
    return old;
end;
$$ language plpgsql;


/**
    @doc    Генерирует тригеры вставки, обновления и удаления,
            для конкретной таблицы переданной как строковый параметр.
**/
create or replace function count_children_on(t varchar) returns void as $$
begin
    execute 'drop trigger if exists count_children_on_update on '||t||';
    create trigger count_children_on_update before update
    on '||t||' for each row execute procedure count_children_on_update();
    
    drop trigger if exists count_children_on_insert on '||t||'  ;
    create trigger count_children_on_insert after insert
    on '||t||' for each row execute procedure count_children_on_insert();

    drop trigger if exists count_children_on_delete on '||t||'  ;
    create trigger count_children_on_delete after delete
    on '||t||' for each row execute procedure count_children_on_delete();';
    return;
end;
$$ language plpgsql;


select count_children_on('doc');
    --- тригеры для подсчета дочерних элементов документов.
select count_children_on('topic');
    --- тригеры для подсчета дочерних элементов дерева тем.
select count_children_on('thingtype');
    --- тригеры для подсчета дочерних элементов дерева типов вещей.
    


/**
    @doc Возвращает начальное состояние пользователя
**/
create or replace function pers_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Статус online \ offline
    **/
    new.pstatus_alias   = 'offline';
    new.pstatus_id      = 
        (select id from pstatus   where alias = new.pstatus_alias);
    /**
        Авторитет пользователя
    **/
    new.authority_alias = 'noob';
    new.authority_id    = 
        (select id from authority where alias = new.authority_alias);
    /**
        Эмоции пользователя
    **/
    new.emotion_alias   = 'indifferent';
    new.emotion_id      = 
        (select id from emotion   where alias = new.emotion_alias);
    /**
        Семейное положения пользователя
    **/
    new.mstatus_alias   = 'single';
    new.mstatus_id      = 
        (select id from mstatus   where alias = new.mstatus_alias);
    /**
        Язык пользователя
    **/
    new.lang_alias      = 'en_gb';
    new.lang_id         = 
        (select id from lang      where alias = new.lang_alias);
    new.room_id         = (select noobsroom());
    return new;
end;
$$ language plpgsql;


/**
    Тригер начального состояния пользователя
**/
drop trigger if exists pers_util_fields_on_insert on pers ;
create trigger pers_util_fields_on_insert before insert
on pers for each row execute procedure pers_util_fields_on_insert();
     

/**
    @doc Обеспечивает совместное состояние документа
**/
create or replace function pers_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Статус online \ offline
    **/
    if new.pstatus_id != old.pstatus_id then
        new.pstatus_alias = 
            (select pstatus.alias from pstatus where pstatus.id = new.pstatus_id);
    end if;
    if new.pstatus_alias != old.pstatus_alias then
        new.pstatus_id = 
            (select pstatus.id from pstatus where pstatus.alias = new.pstatus_alias);
    end if;
    /**
        Авторитет пользователя
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
        Эмоции пользователя
    **/
    if new.emotion_id != old.emotion_id then
        new.emotion_alias = 
            (select emotion.alias 
                from 
                    emotion 
                where 
                    emotion.id = new.emotion_id);
    end if;
    if new.emotion_alias != old.emotion_alias then
        new.emotion_id = 
            (select emotion.id 
                from 
                    emotion 
                where 
                    emotion.alias = new.emotion_alias);
    end if;
    /**
        Семейное положения пользователя
    **/
    if new.mstatus_id != old.mstatus_id then
        new.mstatus_alias = 
            (select mstatus.alias 
                from 
                    mstatus 
                where 
                    mstatus.id = new.mstatus_id
            );
    end if;
    if new.mstatus_alias != old.mstatus_alias then
        new.mstatus_id = 
            (select mstatus.id 
                from 
                    mstatus 
                where 
                    mstatus.alias = new.mstatus_alias
            );
    end if;
    /**
        Язык пользователя
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
    
    return new;
end;
$$ language plpgsql;

     
     
drop trigger if exists pers_util_fields_on_update on pers ;
create trigger pers_util_fields_on_update before update
on pers for each row execute procedure pers_util_fields_on_update();
     
/**
    @doc Возвращает начальное состояние документа
**/
create or replace function doc_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Непросмотрен, разрешен, запрещен, там где это нужно,
    **/
    new.oktype_alias        = 'ncons';
    new.oktype_id           = 
        (select id from oktype    where alias = new.oktype_alias);
    /**
        Разрешение на чтение
    **/
    new.read_acctype_alias  = 'public';
    new.read_acctype_id     = 
        (select id from acctype   where alias = new.read_acctype_alias); 
    /**
        Разрешение комментов
    **/
    new.comm_acctype_alias  = 'private';
    new.comm_acctype_id     = 
        (select id from acctype   where alias = new.comm_acctype_alias);
    /**
        Типы контента: Обычный, эротический
    **/
    new.contype_alias       = 'common';
    new.contype_id          = 
        (select id from contype   where alias = new.contype_alias);
    return new;
end;
$$ language plpgsql;


drop trigger if exists doc_util_fields_on_insert on doc ;
create trigger doc_util_fields_on_insert before insert
on doc for each row execute procedure doc_util_fields_on_insert();
     

/**
    @doc Обеспечивает совместное состояние документа
**/
create or replace function doc_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Непросмотрен, разрешен, запрещен, там где это нужно,
    **/
    if new.oktype_id != old.oktype_id then
        new.oktype_alias = 
            (select oktype.alias from oktype where oktype.id = new.oktype_id);
    end if;
    if new.oktype_alias != old.oktype_alias then
        new.oktype_id = 
            (select oktype.id from oktype where oktype.alias = new.oktype_alias);
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
            (select read_acctype.alias 
                from 
                    read_acctype 
                where 
                    read_acctype.id = new.read_acctype_id);
    end if;
    if new.read_acctype_alias != old.read_acctype_alias then
        new.read_acctype_id = 
            (select read_acctype.id 
                from 
                    read_acctype 
                where 
                    read_acctype.alias = new.read_acctype_alias);
    end if;
    /**
        Разрешение комментов
    **/
    if new.comm_acctype_id != old.comm_acctype_id then
        new.comm_acctype_alias = 
            (select comm_acctype.alias 
                from 
                    comm_acctype 
                where 
                    comm_acctype.id = new.comm_acctype_id
            );
    end if;
    if new.comm_acctype_alias != old.comm_acctype_alias then
        new.comm_acctype_id = 
            (select comm_acctype.id 
                from 
                    comm_acctype 
                where 
                    comm_acctype.alias = new.comm_acctype_alias
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
    
    return new;
end;
$$ language plpgsql;


drop trigger if exists doc_util_fields_on_update on doc ;
create trigger doc_util_fields_on_update before update
on doc for each row execute procedure doc_util_fields_on_update();
     
     
     
/**
    @room Обеспечивает совместное состояние комнаты
**/
create or replace function room_util_fields_on_update() returns "trigger" as $$
begin
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
    return new;
end;
$$ language plpgsql;


drop trigger if exists room_util_fields_on_update on room ;
create trigger room_util_fields_on_update before update
on room for each row execute procedure room_util_fields_on_update();

     
create or replace function room_util_fields_on_insert() returns "trigger" as $$
begin
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
    return new;
end;
$$ language plpgsql;


drop trigger if exists room_util_fields_on_insert on room ;
create trigger room_util_fields_on_insert before insert
on room for each row execute procedure room_util_fields_on_insert();


/**
    @doc Обеспечивает совместное состояние аттача
**/
create or replace function attach_util_fields_on_update() returns "trigger" as $$
begin
    /**
    *  Типы сообщества
    **/
    if new.attachtype_id != old.attachtype_id then
        new.attachtype_alias = 
            (select attachtype.alias 
                from 
                    attachtype 
                where 
                    attachtype.id = new.attachtype_id);
    end if;
    if new.attachtype_alias != old.attachtype_alias then
        new.attachtype_id = 
            (select attachtype.id 
                from 
                    attachtype 
                where 
                    attachtype.alias = new.attachtype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists attach_util_fields_on_update on attach ;
create trigger attach_util_fields_on_update before update
on attach for each row execute procedure attach_util_fields_on_update();


create or replace function attach_util_fields_on_insert() returns "trigger" as $$
begin
    /**
    *  Типы аттачей
    **/
    if (not (new.attachtype_id is null)) and (new.attachtype_alias is null) then
        new.attachtype_alias = 
            (select attachtype.alias 
                from 
                    attachtype 
                where 
                    attachtype.id = new.attachtype_id);
    end if;
    if (new.attachtype_id is null) and (not (new.attachtype_alias is null)) then
        new.attachtype_id = 
            (select attachtype.id 
                from 
                    attachtype 
                where 
                    attachtype.alias = new.attachtype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists attach_util_fields_on_insert on attach ;
create trigger attach_util_fields_on_insert before insert
on attach for each row execute procedure attach_util_fields_on_insert();


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


drop trigger if exists community_util_fields_on_update on community ;
create trigger community_util_fields_on_update before update
on community for each row execute procedure community_util_fields_on_update();


create or replace function community_util_fields_on_insert() returns "trigger" as $$
begin
    /**
    *  Типы сообщества
    **/
    if (not (new.communitytype_id is null)) and (new.communitytype_alias is null) then
        new.communitytype_alias = 
            (select communitytype.alias 
                from 
                    communitytype 
                where 
                    communitytype.id = new.communitytype_id);
    end if;
    if (new.communitytype_id is null) and (not (new.communitytype_alias is null)) then
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


drop trigger if exists community_util_fields_on_insert on community ;
create trigger community_util_fields_on_insert before insert
on community for each row execute procedure community_util_fields_on_insert();



/**
    --------------------------------------------------------------------------
        @doc Обеспечивает совместное состояние cooбщения
    --------------------------------------------------------------------------
**/
create or replace function message_util_fields_on_update() returns "trigger" as $$
begin
    /**
    *  Типы сообщества
    **/
    if new.messagetype_id != old.messagetype_id then
        new.messagetype_alias = 
            (select messagetype.alias 
                from 
                    messagetype 
                where 
                    messagetype.id = new.messagetype_id);
    end if;
    if new.messagetype_alias != old.messagetype_alias then
        new.messagetype_id = 
            (select messagetype.id 
                from 
                    messagetype 
                where 
                    messagetype.alias = new.messagetype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists message_util_fields_on_update on message ;
create trigger message_util_fields_on_update before update
on message for each row execute procedure message_util_fields_on_update();


/**
    @doc Обеспечивает совместное состояние cooбщества
**/

create or replace function event_util_fields_on_update() returns "trigger" as $$
begin
    /**
    *  Типы сообщества
    **/
    if new.eventtype_id != old.eventtype_id then
        new.eventtype_alias = 
            (select eventtype.alias 
                from 
                    eventtype 
                where 
                    eventtype.id = new.eventtype_id);
    end if;
    if new.eventtype_alias != old.eventtype_alias then
        new.eventtype_id = 
            (select eventtype.id 
                from 
                    eventtype 
                where 
                    eventtype.alias = new.eventtype_alias);
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists event_util_fields_on_update on event ;
create trigger event_util_fields_on_update before update
on event for each row execute procedure event_util_fields_on_update();


create or replace function event_util_fields_on_insert() returns "trigger" as $$
begin
    /**
    *  Типы событий
    **/
    if (not (new.eventtype_id is null)) and (new.eventtype_alias is null) then
        new.eventtype_alias = 
            (select eventtype.alias 
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
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists event_util_fields_on_insert on event ;
create trigger event_util_fields_on_insert before insert
on event for each row execute procedure event_util_fields_on_insert();


