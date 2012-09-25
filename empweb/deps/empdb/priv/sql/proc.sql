create language plpgsql;

/**
    Aтомарное создание комнаты для новичков.
**/
create or replace function  mknoobsroom() returns numeric as $$
declare
     _doc_id numeric;
begin
    lock table room  in exclusive mode;
        insert into doc (
            "head", 
            "body"
        ) values (
            'head', 
            'body'
        ) returning id into _doc_id;    
    begin
        insert into room (
            doc_id, 
            type_id
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
            roomtype.id = room.type_id 
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

