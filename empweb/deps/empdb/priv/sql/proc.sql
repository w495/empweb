/**
 * \file В этом файле будем держать описание хранимых процедур,
 * если они нам вообще потребуются
**/

create trusted procedural language 'plpgsql';


/**
 * Тригер для автоматического пополнения кошелька
 *      в случае создания платежа
 * 
**/

/*
create function
    trigge_after_insert_bill ()
        returns trigger as
        '
            begin
                if (new.product_id == null and new.product_type_id == null )>0
                then update purse set sum = sum + new.sum where new.purse_id == purse.id;
                end if;
                return new;
            end;
        ' language  plpgsql;


create trigger
    trigge_after_insert_bill
        before
            insert on
                bill
        for each row
        execute procedure
            trigge_after_insert_bill();*/
