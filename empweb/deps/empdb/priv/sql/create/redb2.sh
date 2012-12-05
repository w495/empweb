# /usr/bin/env bash

##
##  \file Утилита для приведения базы в начальный вид.
##  Созадает схему, просудуры, начальные данные.
##
##  Рядом в папке должные лежать:
##      * Схема базы данных
##              ./${dbname}/${dbname}_scheme.sql
##      * Хранимые процедуры и триггеры
##              ./${dbname}/${dbname}_procedures.sql
##      * Начальные данные для базы 
##              ./${dbname}/${dbname}_initdata.sql
##      * Индексы для ускорения быстродействия
##              ./${dbname}/${dbname}_indexes.sql
##
##  В качестве аргумента можно указать имя базы данных.
##      ./redb.sh ${dbname}
##  Если не задано имя базы, то пересоздаются обе базы данных,
##  необходимые для приложения: emp и ejabberd.
##

redb_dropdb() {
    echo "==============[DROPDB]==============" >> $logfname;
    if dropdb ${dbname} &>> $logfname
    then
        ## Если база есть, то удаляем.
        echo "      ${dbname}:dropdb        OK";
    fi
}

redb_createdb() {
    echo "==============[CREATEDB]==============" >> $logfname;
    if createdb ${dbname} &>> $logfname
    then
        echo "      ${dbname}:createdb      OK";
    else
        echo "      ${dbname}:createdb      ERROR";
        echo "[LOG]:";
        tail $logfname;
        exit 134;
    fi
}

redb_scheme() {
    echo "==============[SCHEME]==============" >> $logfname;
    if psql -d ${dbname} -f $schemefname \
        -v DIRPATH=$(dirname "$schemefname") \
        -v SECTIONPATH=${schemefname%.*} \
        &>> $logfname
    then
        echo "      ${dbname}:scheme        OK";
    else
        echo "      ${dbname}:scheme        ERROR";
        echo "[LOG]:";
        tail $logfname;
        exit 133;
    fi
}

redb_procedures() {
    echo "==============[PROCEDURES]==============" >> $logfname;
    if psql -d ${dbname} -f $proceduresfname \
        -v DIRPATH=$(dirname "$proceduresfname") \
        -v SECTIONPATH=${proceduresfname%.*} \
        &>> $logfname
    then
        echo "      ${dbname}:procedures    OK";
    else
        echo "      ${dbname}:procedures    ERROR";
        echo "[LOG]:";
        tail $logfname;
        exit 132;
    fi
}

redb_initdata() {
    echo "==============[INITDATA]==============" >> $logfname
    if psql -d ${dbname} -f $initdatafname \
        -v DIRPATH=$(dirname "$initdatafname") \
        -v SECTIONPATH=${initdatafname%.*} \
        &>> $logfname
    then
        echo "      ${dbname}:initdata      OK";
    else
        echo "      ${dbname}:initdata      ERROR";
        echo "[LOG]:";
        tail $logfname;
        exit 131;
    fi
}

redb_indexes() {
    echo "==============[INITDATA]==============" >> $logfname
    if psql -d ${dbname} -f $indexesfname  \
        -v DIRPATH=$(dirname "$indexesfname") \
        -v SECTIONPATH=${indexesfname%.*} \
        &>> $logfname
    then
        echo "      ${dbname}:indexes       OK";
    else
        echo "      ${dbname}:indexes       ERROR";
        echo "[LOG]:"
        tail $logfname;
        exit 130;
    fi
}

redb_trylog() {
    if cat $logfname | grep -A2 -n -T -i "ERROR:"
    then
        echo "      ${dbname}:trylog        ERROR";
    else
        echo "      ${dbname}:trylog        OK";
    fi
}

redb_all() {
    redb_dropdb \
        && redb_createdb \
        && redb_scheme \
        && redb_procedures \
        && redb_initdata \
        && redb_indexes \
        && redb_trylog;
}

###########################################################################
##                          MAIN
###########################################################################

##
## Вычисляем абсолюный путь,
## чтобы можно было запускать скрипт из любой директории.
##
abspath=$(cd ${0%/*} && echo $PWD);

##
## Если не задано имя базы, то пересоздаются обе базы данных,
## необходимые для приложения: emp и ejabberd.
##
if [ -z "$1" ]
then
    ## Последовательно
    #   $0 ejabberd;
    #   $0 emp;
    ## Параллельно
    echo "ejabberd emp" |  xargs -d" " -L 1 -P 0 "$0"
    exit 0;
else
    case $1 in
        '--help' | '-h' )
            echo "usage:"
            echo "  $0  [<dbname> [<action>]]"
            echo "action:"
            echo "  'all'          redb_all"
            echo "  'dropdb'       redb_dropdb"
            echo "  'createdb'     redb_createdb"
            echo "  'scheme'       redb_scheme"
            echo "  'procedures'   redb_procedures"
            echo "  'initdata'     redb_initdata"
            echo "  'indexes'      redb_indexes"
            echo "  'trylog'       redb_trylog"
            echo "example:"
            echo "  $0          -> $0 emp && $0 ejabberd "
            echo "  $0 <dbname> -> $0 <dbname> all"
            exit 0;;
        * )
            dbname=`echo "$1" | tr -d ' '` ;
            if [ -z "$2" ]
            then
                ## Последовательно
                #   $0 ejabberd;
                #   $0 emp;
                ## Параллельно
                action='all'
            else
                action=$2;
            fi;;
    esac
fi

##
## Вычисляем абсолюные пути нужных sql-файлов
##
schemefname=${abspath}/${dbname}/${dbname}_scheme.sql;
proceduresfname=${abspath}/${dbname}/${dbname}_procedures.sql;
initdatafname=${abspath}/${dbname}/${dbname}_initdata.sql;
indexesfname=${abspath}/${dbname}/${dbname}_indexes.sql;
logfname=${abspath}/${dbname}.log;

##
## Логируем свои действия
##
echo "==============[INFO]=============="               &>  $logfname
echo "dbname        ${dbname}"                          &>> $logfname
echo "date          " `date "+%Y-%m-%d_%H-%M-%S-%N"`    &>> $logfname;
echo "scheme        ${schemefname}"                     &>> $logfname;
echo "procedures    ${proceduresfname}"                 &>> $logfname;
echo "initdata      ${initdatafname}"                   &>> $logfname;
echo "indexes       ${indexesfname}"                    &>> $logfname;
echo "log           ${logfname}"                        &>> $logfname;

echo "<${dbname}>";
case $action in
    'all' )         redb_all;;
    'dropdb'  )     redb_dropdb;;
    'createdb' )    redb_createdb;;
    'scheme' )      redb_scheme;;
    'procedures' )  redb_procedures;;
    'initdata' )    redb_initdata;;
    'indexes' )     redb_indexes;;
    'trylog' )      redb_trylog;;
esac
echo "</${dbname}>";
exit 0;






