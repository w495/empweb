# /usr/bin/env bash

##
##  \file Утилита для создания дампа базы
##  Создается дамп всех баз, необходимых приложению,  заданной базы данных 
##  (1 аргумент) или конкретной таблицы заданной базы данных (2 аргумента)
##
##  В качестве аргумента можно указать имя базы данных.
##      ./mkdump ${dbname}
##  A можно указать имя базы данных и кокретной таблицы
##      ./mkdump ${dbname} ${tbname}
##  Если не задано имя базы, то создается дамп обоих баз emp и ejabberd.
##

options="--no-privileges --no-owner --no-reconnect "; # --data-only --inserts
tbname=$2;

## Вычисляем абсолюный путь,
## чтобы можно было запускать скрипт из любой директории.
abspath=$(cd ${0%/*} && echo $PWD);


## Если не задано имя базы, то создается дамп обоих баз emp и ejabberd.
if [ -z "$1" ]
then
    ## Последовательно
    #   $0 ejabberd;
    #   $0 emp;
    ## Параллельно
    echo "ejabberd emp" |  xargs -d" " -L 1 -P 0 "$0"
    ## Параллельность нужна, т.к. при значительном объеме бызы
    ## чтобы дампы создавались и архивировались одновременно
    exit 0;
else
    dbname=`echo "$1" | tr -d ' '` ;
fi

##
## main() ->
##
    echo "<${dbname}>";
    ## Если не указано имя таблицы, то создается дамп всей базы данных.
    ## Если имя таблицы указано, то создаем дамп только для указанной таблицы.
    if [ -z "${tbname}" ]
    then
        dumpfname=${abspath}/dump-${dbname}-`date "+%Y-%m-%d_%H-%M-%S-%N"`.sql;
        ## Создаем дамп всей базы данных
        pg_dump ${dbname} ${options} > "${dumpfname}"
        if [ 0 == $? ]
        then
            echo    "      ${dbname}:dump          OK";
            ## Сжимаем полученный дамп
            bzip2   "${dumpfname}";
            if [ 0 == $? ]
            then
                echo    "      ${dbname}:bzip2         OK";
            else
                echo    "      ${dbname}:bzip2         ERROR";
            fi
        else
            echo    "      ${dbname}:dump          ERROR"
        fi
    else
        echo "  <${dbname}:${tbname}>";
        dumpfname=${abspath}/dump-${dbname}-`date "+%Y-%m-%d_%H-%M-%S-%N"`.${tbname}.sql
        ## Создаем дамп конкретной таблицы базы данных
        pg_dump --table=${tbname} ${dbname} ${options} > "${dumpfname}"s
        if [ 0 == $? ]
        then
            echo    "      ${dbname}:dump          OK";
            ## Сжимаем полученный дамп
            bzip2   "${dumpfname}";
            if [ 0 == $? ]
            then
                echo    "      ${dbname}:bzip2         OK";
            else
                echo    "      ${dbname}:bzip2         ERROR";
            fi
        else
            echo    "      ${dbname}:dump          ERROR"
        fi
        echo    "  </${dbname}:${tbname}>";
    fi
    echo "</${dbname}>";
    exit 0;


