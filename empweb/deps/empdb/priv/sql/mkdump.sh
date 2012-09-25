`# /usr/bin/env bash

## 
##  \file Утилита для создания дампа базы
##

##
##  В качестве аргумента можно указать имя базы данных.
##      ./mkdump my_db_name
##
##

default_db_name="fk"
default_tb_name="customer"

options="--no-privileges --no-owner --no-reconnect " # --data-only --inserts


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
    if [ -z "$2" ]
    then
        pg_dump $db_name $options  \
            > dump-$db_name-`date "+%Y-%m-%d_%H-%M-%S-%N"`.sql
    else
        pg_dump --table=$2 $db_name $options \
            > dump-$db_name-`date "+%Y-%m-%d_%H-%M-%S-%N"`.$2.sql
    fi



