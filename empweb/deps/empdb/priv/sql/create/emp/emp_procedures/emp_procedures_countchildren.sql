\echo :FILE 'in'


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
                nnodes = nnodes + '||
                    cast((new.nnodes - old.nnodes) as varchar) ||'
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
select count_children_on('geo');
    --- тригеры для подсчета дочерних элементов дерева типов вещей.


\echo :FILE ok
