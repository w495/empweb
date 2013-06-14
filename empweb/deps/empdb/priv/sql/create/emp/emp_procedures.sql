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
    @doc присвоения типа документа
**/
\set FILE :SECTIONPATH /emp_procedures_subdoc.sql \i :FILE

/**
    @doc Файлы
**/
\set FILE :SECTIONPATH /emp_procedures_countchildren.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_tr.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_pers.sql \i :FILE

/**
    @doc История сообщества
**/
\set FILE :SECTIONPATH /emp_procedures_vote.sql \i :FILE


/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_doc.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_room.sql \i :FILE



/**
    @doc Действия пользователя
**/
\set FILE :SECTIONPATH /emp_procedures_roomlist.sql \i :FILE



/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_attach.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_community.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_message.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_event.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_thing.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_thingbuy.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_thingwish.sql \i :FILE


/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_experbuy.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_roomlot.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_roombet.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_communitylot.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_communitybet.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_pay.sql \i :FILE

/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_roomtreas.sql \i :FILE


/**
    @doc
**/
\set FILE :SECTIONPATH /emp_procedures_communitytreas.sql \i :FILE


/**
    @doc Денежные переводы между пользователями и комнатами
**/
\set FILE :SECTIONPATH /emp_procedures_rptrans.sql \i :FILE


/**
    @doc Денежные переводы между пользователями и комнатами
**/
\set FILE :SECTIONPATH /emp_procedures_cptrans.sql \i :FILE


/**
    @doc Файловые сущности
**/
\set FILE :SECTIONPATH /emp_procedures_file.sql \i :FILE

/**
    @doc Информация о файлах
**/
\set FILE :SECTIONPATH /emp_procedures_fileinfo.sql \i :FILE

/**
    @doc Друзья
**/
\set FILE :SECTIONPATH /emp_procedures_friend.sql \i :FILE

/**
    @doc Календарь
**/
\set FILE :SECTIONPATH /emp_procedures_notice.sql \i :FILE

/**
    @doc История сообщества
**/
\set FILE :SECTIONPATH /emp_procedures_communityhist.sql \i :FILE

/**
    @doc Действия пользователя
**/
\set FILE :SECTIONPATH /emp_procedures_action.sql \i :FILE

/**
    @doc Действия пользователя
**/
\set FILE :SECTIONPATH /emp_procedures_exile.sql \i :FILE

/**
    @doc Действия пользователя
**/
\set FILE :SECTIONPATH /emp_procedures_zprotbuy.sql \i :FILE





