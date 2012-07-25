# /usr/bin/env bash

## 
##  \file Утилита для приведения базы в начальный вид.
##  Созадает схему, просудуры, начальные данные.
##

##  Рядом в папке должные лежать:
##      # ./scheme.sql, для схем;
##      # ./proc.sql,	для процедур;
##      # ./insert.sql,	для данных.
##
##  В качестве аргумента можно указать имя базы данных.
##      ./redb.sh my_db_name
##
##  !!! ВНИМАНИЕ:
##      Лучше после применения смотреть log.
##      Ошибки связанные со скриптами
##      не будут отловлены.
##

# default_db_name=adv_system

default_db_name=emp

scheme_file_name=./scheme.sql
proc_file_name=./proc.sql
insert_file_name=./insert.sql
geo_file_name=./geo.sql
log_file_name=./log.log


## 
## \fn Процедура создания базы
##
##	p_createdb() ->
## 		createdb,
## 		psql < ./scheme.sql,
##		psql < ./proc.sql,
##		psql < ./insert.sql.
##
p_createdb() {
	if createdb $db_name &>> $log_file_name
	then
		echo "База была успешно создана. Сейчас создадим схему ..."
		echo "==============[SCHEME]==============" >> $log_file_name
		if psql -d $db_name < $scheme_file_name &>> $log_file_name
		then
			echo "Схема была принята. Сейчас создадим процедуры ..."
			echo "==============[PROC]==============" >> $log_file_name
			if psql -d $db_name < $proc_file_name &>> $log_file_name
			then
				echo "Процедуры были созданы. Сейчас вставим данные ..."
				echo "==============[DATA]==============" >> $log_file_name
				if psql -d $db_name < $insert_file_name &>> $log_file_name
                then
                    echo "Данные были вставлены. Сейчас вставим регионы  ..."
                    echo "==============[DATA]==============" >> $log_file_name
                    if psql -d $db_name < $geo_file_name &>> $log_file_name
                    then
                        echo "Данные были вставлены."
                        echo "Проверка log."
                        if cat $log_file_name | grep -A2 -n -T -i "ERROR:"
                        then
                            echo "Хинт: cтрочки выше говорят об ошибках."
                        else
                            echo "Все кончилось хорошо."
                        fi
                    else
                        echo "Регионы  НЕ были вставлены. Смотрите почему:"
                        echo "----------[tail log:]---------------------"
                        tail $log_file_name
                    fi
                else
                    echo "Данные НЕ были вставлены. Смотрите почему:"
                    echo "----------[tail log:]---------------------"
                    tail $log_file_name
                fi
			else
				echo "Процедуры НЕ были созданы. Смотрите почему:"
				echo "----------[tail log:]---------------------"
				tail $log_file_name
			fi
		else
			echo "Схема НЕ была принята. Смотрите почему:"
			echo "----------[tail log:]---------------------"
			tail $log_file_name
		fi
	else
		echo "База была успешно НЕ создана. Смотрите почему:"
		echo "----------[tail log:]---------------------"
		tail log
	fi
	echo "------------------------------------------"
}

##
## main() ->
##
    db_name=$default_db_name
    if [ -z "$1" ]
    then
        db_name=$default_db_name
    else
        db_name=$1
    fi
    echo "Используем $db_name в качестве базы данных."
    if dropdb $db_name &> $log_file_name
    then
        ## Если база есть, то удаляем.
        echo "База была успешно удалена. Сейчас мы ее создадим ..."
        p_createdb
    else
        ## Если базы нет, то просто создаем.
        echo "Сейчас мы создадим базу ..."
        p_createdb
    fi
