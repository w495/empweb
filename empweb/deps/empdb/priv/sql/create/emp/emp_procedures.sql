/**
    @doc Выполняет кроссверсионный вариант
    create or replace language plpgsql;
**/

create or replace function make_plpgsql()
returns void
language sql
as $$
create language plpgsql;
$$;

select
    case
    when exists(
        select 1
        from pg_catalog.pg_language
        where lanname='plpgsql'
    )
    then null
    else make_plpgsql() end;

drop function make_plpgsql();


/**
    Тригер присвоения типа документа при создании чего бы то ни было.
    пока не нужно.
**/
-- create or replace function update_something() returns "trigger" as $$
-- begin
--     new.updated = now();
--     new.nupdates = new.nupdates + 1;
--     return new;
-- end;
-- $$ language plpgsql;
-- 
-- create trigger update_something before update
--    on doc for each row execute procedure update_something();



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
    Aтомарное создание комнаты для новичков.
**/

create sequence seq_noobslive_room_id;
create or replace function  mknoobsroom() returns numeric as $$
declare
     _res numeric;
begin
        select mknoobsroom((select -nextval('seq_noobslive_room_id'))) into _res;
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
    execute 'drop trigger if exists t1count_children_on_update on '||t||';
    create trigger t1count_children_on_update before update
    on '||t||' for each row execute procedure count_children_on_update();
    
    drop trigger if exists t1count_children_on_insert on '||t||'  ;
    create trigger t1count_children_on_insert after insert
    on '||t||' for each row execute procedure count_children_on_insert();

    drop trigger if exists t1count_children_on_delete on '||t||'  ;
    create trigger t1count_children_on_delete after delete
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
        Опыт пользователя
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
    new.live_room_id         = (select noobsroom());
    new.live_room_head       = (select doc.head from doc where doc.id = new.live_room_id and doc.doctype_alias = 'room');

    /**
        Положение пользователя в стране
    **/
    if (new.live_room_pos  is null) then
        new.live_room_pos = cast(
            cast (new.live_room_id as varchar(1024))
            || '.'
            ||  cast ((
                    select count(id)
                    from pers
                    where pers.live_room_id = new.live_room_id
                ) as varchar(1024)
            ) as numeric
        );
    end if;
    
    return new;
end;
$$ language plpgsql;

/**
    Тригер начального состояния пользователя
**/
drop trigger if exists t1pers_util_fields_on_insert on pers ;
create trigger t1pers_util_fields_on_insert before insert
on pers for each row execute procedure pers_util_fields_on_insert();

/**
    @doc Обеспечивает совместное состояние документа
**/
create or replace function pers_util_fields_on_update() returns "trigger" as $$
begin
    if new.own_room_id != old.own_room_id then
        new.own_room_head =
            (select doc.head from doc where doc.id = new.own_room_id and doc.doctype_alias = 'room');
    end if;
    if new.live_room_id != old.live_room_id then
        new.live_room_head =
            (select doc.head from doc where doc.id = new.live_room_id and doc.doctype_alias = 'room');
    end if;
    if new.community_id != old.community_id then
        new.community_head =
            (select doc.head from doc where doc.id = new.community_id and doc.doctype_alias = 'community');
    end if;
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


    if new.exper != old.exper then
        /**
            Авторитет пользователя,
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

    if new.live_room_pos != old.live_room_pos then
        if (new.live_room_pos  is null) then
            new.live_room_pos = 100
            cast(
                cast (new.live_room_id as varchar(1024))
                || '.'
                ||  cast ((
                        select count(id)
                        from pers
                        where pers.live_room_id = new.live_room_id
                    ) as varchar(1024)
                ) as numeric
            );
        end if;
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1pers_util_fields_on_update on pers ;
create trigger t1pers_util_fields_on_update before update
on pers for each row execute procedure pers_util_fields_on_update();

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

    if new.head != old.head then
        if new.doctype_alias = 'room' then
            update pers set live_room_head = new.head where pers.live_room_id = new.id;
            update pers set own_room_head = new.head where pers.own_room_id = new.id;
        end if;
        if new.doctype_alias = 'community' then
            update pers set community_head = new.head where pers.community_id = new.id;
        end if;
    end if;
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
    
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1doc_util_fields_on_update on doc ;
create trigger t1doc_util_fields_on_update before update
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


drop trigger if exists t1attach_util_fields_on_update on attach ;
create trigger t1attach_util_fields_on_update before update
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


drop trigger if exists t1attach_util_fields_on_insert on attach ;
create trigger t1attach_util_fields_on_insert before insert
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


drop trigger if exists t1community_util_fields_on_update on community ;
create trigger t1community_util_fields_on_update before update
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


drop trigger if exists t1community_util_fields_on_insert on community ;
create trigger t1community_util_fields_on_insert before insert
on community for each row execute procedure community_util_fields_on_insert();



/**
    --------------------------------------------------------------------------
        @doc Обеспечивает совместное состояние cooбщения
    --------------------------------------------------------------------------
**/


create or replace function message_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Получатель сообщения
    **/
    if (new.reader_nick is null) then
        if not (new.reader_id is null) then
            new.reader_nick =
                (select pers.nick from pers where pers.id = new.reader_id);
        else
            new.reader_nick        = null;
        end if;
    end if;
    if (new.reader_id is null) then
        new.reader_id           =
            (select pers.id from pers where pers.nick = new.reader_nick);
    end if;

    /**
    *  Типы сообщества
    **/
    if (not (new.messagetype_id is null)) and (new.messagetype_alias is null) then
        new.messagetype_alias =
            (select messagetype.alias
                from
                    messagetype
                where
                    messagetype.id = new.messagetype_id);
    end if;
    if (new.messagetype_id is null) and (not (new.messagetype_alias is null)) then
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

drop trigger if exists t1message_util_fields_on_insert on message ;
create trigger t1message_util_fields_on_insert before insert
on message for each row execute procedure message_util_fields_on_insert();

create or replace function message_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Получатель сообщения
    **/
    if new.reader_id != old.reader_id then
        new.reader_nick =
            (select pers.nick from pers where pers.id = new.reader_id);
    end if;
    if new.reader_nick != old.reader_nick then
        new.reader_id =
            (select pers.id from pers where pers.nick = new.reader_nick);
    end if;
    /**
    *  Типы cooбщения
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


drop trigger if exists t1message_util_fields_on_update on message ;
create trigger t1message_util_fields_on_update before update
on message for each row execute procedure message_util_fields_on_update();


/**
    @doc Обеспечивает совместное состояние события
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


drop trigger if exists t1event_util_fields_on_update on event ;
create trigger t1event_util_fields_on_update before update
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


drop trigger if exists t1event_util_fields_on_insert on event ;
create trigger t1event_util_fields_on_insert before insert
on event for each row execute procedure event_util_fields_on_insert();



/**
    @doc Обеспечивает совместное состояние вещей (thing)
**/

create or replace function thing_util_fields_on_update() returns "trigger" as $$
begin
    if new.thingtype_id != old.thingtype_id then
        update thingtype set
            nchildtargets   = nchildtargets + 1
        where
            thingtype.id = new.thingtype_id;
        update thingtype set
            nchildtargets   = nchildtargets - 1
        where
            thingtype.id = new.thingtype_id;
    end if;
    if (new.isdeleted = true) and (old.isdeleted = false) then
        update thingtype set
            nchildtargets   = nchildtargets - 1
        where
            thingtype.id = new.thingtype_id;
    end if;
    if (new.isdeleted = false) and (old.isdeleted = true) then
        update thingtype set
            nchildtargets   = nchildtargets + 1
        where
            thingtype.id = new.thingtype_id;
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1thing_util_fields_on_update on thing ;
create trigger t1thing_util_fields_on_update before update
on thing for each row execute procedure thing_util_fields_on_update();


create or replace function thing_util_fields_on_insert() returns "trigger" as $$
begin
    if not (new.thingtype_id is null) then
        update thingtype set
            nchildtargets   = nchildtargets + 1
        where
            thingtype.id = new.thingtype_id;
    end if;
    return new;
end;
$$ language plpgsql;


drop trigger if exists t1thing_util_fields_on_insert on thing ;
create trigger t1thing_util_fields_on_insert before insert
on thing for each row execute procedure thing_util_fields_on_insert();


create or replace function thing_util_fields_on_delete() returns "trigger" as $$
begin
    if not (old.thingtype_id is null) then
        update thingtype set
            nchildtargets   = nchildtargets - 1
        where
            thingtype.id = old.thingtype_id;
    end if;
    return old;
end;
$$ language plpgsql;


drop trigger if exists t1thing_util_fields_on_delete on thing ;
create trigger t1thing_util_fields_on_delete before delete
on thing for each row execute procedure thing_util_fields_on_delete();

--(2012.10.12 14:38:23:554386209)---------------------------------------------

/**
    @doc Обеспечивает совместное состояние покупок (thingbuy)
**/
create or replace function thingbuy_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Плательщик покупки
    **/
    if (new.buyer_nick is null) then
        if not (new.buyer_id is null) then
            new.buyer_nick =
                (select pers.nick from pers where pers.id = new.buyer_id);
        else
            new.buyer_nick        = null;
        end if;
    end if;
    if (new.buyer_id is null) then
        new.buyer_id           =
            (select pers.id from pers where pers.nick = new.buyer_nick);
    end if;
    /**
        Владелец покупки
    **/
    if (new.owner_nick is null) then
        if not (new.owner_id is null) then
            new.owner_nick =
                (select pers.nick from pers where pers.id = new.owner_id);
        else
            /**
                если владелец не указан, то им становится, тот кто покупает
            **/
            new.owner_nick        = new.buyer_nick;
        end if;
    end if;
    if (new.owner_id is null) then
        new.owner_id           =
            (select pers.id from pers where pers.nick = new.owner_nick);
    end if;
    /**
        Покупаемая вещь
    **/
    if (new.thing_alias is null) then
        if not (new.thing_id is null) then
            new.thing_alias =
                (select thing.alias from thing where thing.id = new.thing_id);
        else
            new.thing_alias        = null;
        end if;
    end if;
    if (new.thing_id is null) then
        new.thing_id           =
            (select thing.id from thing where thing.alias = new.thing_alias);
    end if;
    
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1thingbuy_util_fields_on_insert on thingbuy ;
create trigger t1thingbuy_util_fields_on_insert before insert
on thingbuy for each row execute procedure thingbuy_util_fields_on_insert();

create or replace function thingbuy_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Плательщик покупки
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
        Владелец покупки
    **/
    if new.buyer_id != old.buyer_id then
        new.buyer_nick =
            (select pers.nick from pers where pers.id = new.buyer_id);
    end if;
    if new.buyer_nick != old.buyer_nick then
        new.buyer_id =
            (select pers.id from pers where pers.nick = new.buyer_nick);
    end if;
    /**
        Покупаемая вещь
    **/
    if new.thing_id != old.thing_id then
        new.thing_alias =
            (select thing.alias from thing where thing.id = new.thing_id);
    end if;
    if new.thing_alias != old.thing_alias then
        new.thing_id =
            (select thing.id from thing where thing.alias = new.thing_alias);
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1thingbuy_util_fields_on_update on thingbuy ;
create trigger t1thingbuy_util_fields_on_update before update
on thingbuy for each row execute procedure thingbuy_util_fields_on_update();


--(2012.10.12 14:38:23:554386209)---------------------------------------------


/**
    @doc Обеспечивает совместное состояние покупок авторитетов (experbuy)
**/
create or replace function experbuy_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Плательщик покупки
    **/
    if (new.buyer_nick is null) then
        if not (new.buyer_id is null) then
            new.buyer_nick =
                (select pers.nick from pers where pers.id = new.buyer_id);
        else
            new.buyer_nick        = null;
        end if;
    end if;
    if (new.buyer_id is null) then
        new.buyer_id           =
            (select pers.id from pers where pers.nick = new.buyer_nick);
    end if;
    /**
        Владелец покупки
    **/
    if (new.owner_nick is null) then
        if not (new.owner_id is null) then
            new.owner_nick =
                (select pers.nick from pers where pers.id = new.owner_id);
        else
            /**
                если владелец не указан, то им становится, тот кто покупает
            **/
            new.owner_nick        = new.buyer_nick;
        end if;
    end if;
    if (new.owner_id is null) then
        new.owner_id           =
            (select pers.id from pers where pers.nick = new.owner_nick);
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1experbuy_util_fields_on_insert on experbuy ;
create trigger t1experbuy_util_fields_on_insert before insert
on experbuy for each row execute procedure experbuy_util_fields_on_insert();

create or replace function experbuy_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Плательщик покупки
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
        Владелец покупки
    **/
    if new.buyer_id != old.buyer_id then
        new.buyer_nick =
            (select pers.nick from pers where pers.id = new.buyer_id);
    end if;
    if new.buyer_nick != old.buyer_nick then
        new.buyer_id =
            (select pers.id from pers where pers.nick = new.buyer_nick);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1experbuy_util_fields_on_update on experbuy ;
create trigger t1experbuy_util_fields_on_update before update
on experbuy for each row execute procedure experbuy_util_fields_on_update();


--(2012.10.22 16:01:55:586728391)---------------------------------------------


/**
    @doc Обеспечивает совместное состояние лота аукциона
**/
create or replace function roomlot_util_fields_on_insert() returns "trigger" as $$
begin
    if not (new.room_id is null) then
        new.room_head =
            (select head from doc where doc.id = new.room_id);
    end if;
    new.betcur = new.betmin;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roomlot_util_fields_on_insert on roomlot ;
create trigger t1roomlot_util_fields_on_insert before insert
on roomlot for each row execute procedure roomlot_util_fields_on_insert();


create or replace function roomlot_util_fields_after_insert() returns "trigger" as $$
begin
    if not (new.room_id is null) then
        update room set
            roomlot_id      = new.doc_id,
            roomlot_betmin  = new.betmin,
            roomlot_betmax  = new.betmax,
            roomlot_dtstart = new.dtstart,
            roomlot_dtstop  = new.dtstop
        where
            room.doc_id = new.room_id;
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roomlot_util_fields_after_insert on roomlot ;
create trigger t1roomlot_util_fields_after_insert after insert
on roomlot for each row execute procedure roomlot_util_fields_after_insert();

create or replace function roomlot_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Плательщик покупки
    **/
    if (new.room_id != old.room_id) then
        new.room_head =
            (select head from doc where doc.id = new.room_id);
        if (not (new.room_id is null)) then
            update room set
                roomlot_id      = null,
                roomlot_betmin  = null,
                roomlot_betmax  = null,
                roomlot_dtstart = null,
                roomlot_dtstop  = null
            where
                room.doc_id = old.room_id;
            update room set
                roomlot_id      = new.doc_id,
                roomlot_betmin  = new.betmin,
                roomlot_betmax  = new.betmax,
                roomlot_dtstart = new.dtstart,
                roomlot_dtstop  = new.dtstop
            where
                room.doc_id = new.room_id;
        end if;
    end if;

    if (new.betmin != old.betmin) and (not (new.room_id is null)) then
        update room set
            roomlot_betmin  = new.betmin
        where
            room.doc_id = new.room_id;
    end if;
    
    if (new.betmax != old.betmax) and (not (new.room_id is null)) then
        update room set
            roomlot_betmax  = new.betmax
        where
            room.doc_id = new.room_id;
    end if;
    
    if (new.dtstart != old.dtstart) and (not (new.room_id is null)) then
        update room set
            roomlot_dtstart  = new.dtstart
        where
            room.doc_id = new.room_id;
    end if;
    
    if (new.dtstop != old.dtstop) and (not (new.room_id is null)) then
        update room set
            roomlot_dtstop  = new.dtstop
        where
            room.doc_id = new.room_id;
    end if;
    
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roomlot_util_fields_on_update on roomlot ;
create trigger t1roomlot_util_fields_on_update before update
on roomlot for each row execute procedure roomlot_util_fields_on_update();


/**
    @doc Обеспечивает совместное состояние ставки аукциона
**/
create or replace function roombet_util_fields_on_insert() returns "trigger" as $$
begin
    /**
        Аукцион и предмет аукциона
    **/
    if not (new.roomlot_id is null) then
        new.room_id =
            (select room_id from roomlot where roomlot.doc_id = new.roomlot_id);
        new.room_head =
            (select room_head from roomlot where roomlot.doc_id = new.roomlot_id);
    end if;
    /**
        Владелец заявки
    **/
    if (new.owner_nick is null) then
        if not (new.owner_id is null) then
            new.owner_nick =
                (select pers.nick from pers where pers.id = new.owner_id);
        end if;
    end if;
    if (new.owner_id is null) then
        new.owner_id           =
            (select pers.id from pers where pers.nick = new.owner_nick);
    end if;
    
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roombet_util_fields_on_insert on roombet ;
create trigger t1roombet_util_fields_on_insert before insert
on roombet for each row execute procedure roombet_util_fields_on_insert();


create or replace function roombet_util_fields_after_insert() returns "trigger" as $$
begin
    if not (new.room_id is null) then
        update room set
            roombet_id          = new.id,
            roombet_price       = new.price,
            roombet_owner_id    = new.owner_id,
            roombet_owner_nick  = new.owner_nick
        where
            room.doc_id = new.room_id;
    end if;
    if not (new.roomlot_id is null) then
        update roomlot set
            betcur              = new.price
        where
            roomlot.doc_id = new.roomlot_id;
    end if;
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roombet_util_fields_after_insert on roombet ;
create trigger t1roombet_util_fields_after_insert after insert
on roombet for each row execute procedure roombet_util_fields_after_insert();



create or replace function roombet_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Аукцион и предмет аукциона
    **/
    if new.roomlot_id != old.roomlot_id then
        new.room_id =
            (select room_id from roomlot where roomlot.doc_id = new.roomlot_id);
        new.room_head =
            (select room_head from roomlot where roomlot.doc_id = new.roomlot_id);
    end if;
    /**
        Владелец заявки
    **/
    if new.owner_id != old.owner_id then
        new.owner_nick =
            (select pers.nick from pers where pers.id = new.owner_id);
        update room set
            roombet_owner_nick  = new.owner_nick,
            roombet_owner_id    = new.owner_id
        where
            room.doc_id = new.room_id;
    end if;
    /** ------------------------------------------ **/
    if new.owner_nick != old.owner_nick then
        new.owner_id =
            (select pers.id from pers where pers.nick = new.owner_nick);
        update room set
            roombet_owner_nick  = new.owner_nick,
            roombet_owner_id    = new.owner_id
        where
            room.doc_id = new.room_id;
    end if;
    
    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roombet_util_fields_on_update on roombet ;
create trigger t1roombet_util_fields_on_update before update
on roombet for each row execute procedure roombet_util_fields_on_update();

--(2012.10.25 18:19:03:973218989)---------------------------------------------

/**
    @doc Обеспечивает совместное состояние ставки аукциона
**/
create or replace function pay_util_fields_on_insert() returns "trigger" as $$
begin
    if (new.pers_nick is null) then
        if not (new.pers_id is null) then
            new.pers_nick =
                (select pers.nick from pers where pers.id = new.pers_id);
        end if;
    end if;
    if (new.pers_id is null) then
        new.pers_id           =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if (new.paytype_alias is null) then
        if not (new.paytype_id is null) then
            new.paytype_alias =
                (select paytype.alias from paytype where paytype.id = new.paytype_id);
        end if;
    end if;
    if (new.paytype_id is null) then
        new.paytype_id           =
            (select paytype.id from paytype where paytype.alias = new.paytype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1pay_util_fields_on_insert on pay ;
create trigger t1pay_util_fields_on_insert before insert
on pay for each row execute procedure pay_util_fields_on_insert();

create or replace function pay_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Владелец заявки
    **/
    if new.pers_id != old.pers_id then
        new.pers_nick =
            (select pers.nick from pers where pers.id = new.pers_id);
    end if;
    if new.pers_nick != old.pers_nick then
        new.pers_id =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if new.paytype_id != old.paytype_id then
        new.paytype_alias =
            (select paytype.alias from paytype where paytype.id = new.paytype_id);
    end if;
    if new.paytype_alias != old.paytype_alias then
        new.paytype_id =
            (select paytype.id from paytype where paytype.nick = new.paytype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1pay_util_fields_on_update on pay ;
create trigger t1pay_util_fields_on_update before update
on pay for each row execute procedure pay_util_fields_on_update();

--(2012.11.05 22:40:59:888957862)---------------------------------------------


/**
    @doc Обеспечивает совместное состояние лога прихода\расхода страны
**/
create or replace function roomtreas_util_fields_on_insert() returns "trigger" as $$
begin
    if (new.pers_nick is null) then
        if not (new.pers_id is null) then
            new.pers_nick =
                (select pers.nick from pers where pers.id = new.pers_id);
        end if;
    end if;
    if (new.pers_id is null) then
        new.pers_id           =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if (new.treastype_alias is null) then
        if not (new.treastype_id is null) then
            new.treastype_alias =
                (select treastype.alias from treastype where treastype.id = new.treastype_id);
        end if;
    end if;
    if (new.treastype_id is null) then
        new.treastype_id           =
            (select treastype.id from treastype where treastype.alias = new.treastype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roomtreas_util_fields_on_insert on roomtreas ;
create trigger t1roomtreas_util_fields_on_insert before insert
on roomtreas for each row execute procedure roomtreas_util_fields_on_insert();

create or replace function roomtreas_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Владелец заявки
    **/
    if new.pers_id != old.pers_id then
        new.pers_nick =
            (select pers.nick from pers where pers.id = new.pers_id);
    end if;
    if new.pers_nick != old.pers_nick then
        new.pers_id =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if new.treastype_id != old.treastype_id then
        new.treastype_alias =
            (select treastype.alias from treastype where treastype.id = new.treastype_id);
    end if;
    if new.treastype_alias != old.treastype_alias then
        new.treastype_id =
            (select treastype.id from treastype where treastype.nick = new.treastype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1roomtreas_util_fields_on_update on roomtreas ;
create trigger t1roomtreas_util_fields_on_update before update
on roomtreas for each row execute procedure roomtreas_util_fields_on_update();




/**
    @doc Обеспечивает совместное состояние денежного перевода для страны
**/
create or replace function rptrans_util_fields_on_insert() returns "trigger" as $$
begin
    if (new.pers_nick is null) then
        if not (new.pers_id is null) then
            new.pers_nick =
                (select pers.nick from pers where pers.id = new.pers_id);
        end if;
    end if;
    if (new.pers_id is null) then
        new.pers_id           =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if (new.transtype_alias is null) then
        if not (new.transtype_id is null) then
            new.transtype_alias =
                (select transtype.alias from transtype where transtype.id = new.transtype_id);
        end if;
    end if;
    if (new.transtype_id is null) then
        new.transtype_id           =
            (select transtype.id from transtype where transtype.alias = new.transtype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1rptrans_util_fields_on_insert on rptrans ;
create trigger t1rptrans_util_fields_on_insert before insert
on rptrans for each row execute procedure rptrans_util_fields_on_insert();

create or replace function rptrans_util_fields_on_update() returns "trigger" as $$
begin
    /**
        Владелец заявки
    **/
    if new.pers_id != old.pers_id then
        new.pers_nick =
            (select pers.nick from pers where pers.id = new.pers_id);
    end if;
    if new.pers_nick != old.pers_nick then
        new.pers_id =
            (select pers.id from pers where pers.nick = new.pers_nick);
    end if;

    if new.transtype_id != old.transtype_id then
        new.transtype_alias =
            (select transtype.alias from transtype where transtype.id = new.transtype_id);
    end if;
    if new.transtype_alias != old.transtype_alias then
        new.transtype_id =
            (select transtype.id from transtype where transtype.nick = new.transtype_alias);
    end if;

    return new;
end;
$$ language plpgsql;

drop trigger if exists t1rptrans_util_fields_on_update on rptrans ;
create trigger t1rptrans_util_fields_on_update before update
on rptrans for each row execute procedure rptrans_util_fields_on_update();


