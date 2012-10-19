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
    dbname=`echo "$1" | tr -d ' '` ;
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
echo "<${dbname}>";
echo "==============[INFO]=============="               &>  $logfname
echo "dbname        ${dbname}"                          &>> $logfname
echo "date          " `date "+%Y-%m-%d_%H-%M-%S-%N"`    &>> $logfname;
echo "scheme        ${schemefname}"                     &>> $logfname;
echo "procedures    ${proceduresfname}"                 &>> $logfname;
echo "initdata      ${initdatafname}"                   &>> $logfname;
echo "indexes       ${indexesfname}"                    &>> $logfname;
echo "log           ${logfname}"                        &>> $logfname;


##
## \fn Процедура создания базы
##
##	p_createdb() ->
##      createdb ${dbname};
##      psql < ./${dbname}/${dbname}_scheme.sql;
##      psql < ./${dbname}/${dbname}_procedures.sql;
##      psql < ./${dbname}/${dbname}_initdata.sql;
##      psql < ./${dbname}/${dbname}_indexes.sql;
##
p_createdb() {
    if createdb ${dbname} &>> $logfname
    then
        echo "      ${dbname}:createdb      OK";
        echo "==============[SCHEME]==============" >> $logfname
        if psql -d ${dbname} < $schemefname &>> $logfname
        then
            echo "      ${dbname}:scheme        OK";
            echo "==============[PROCEDURES]==============" >> $logfname
            if psql -d ${dbname} < $proceduresfname &>> $logfname
            then
                echo "      ${dbname}:procedures    OK";
                echo "==============[DATA]==============" >> $logfname
                if psql -d ${dbname} < $initdatafname &>> $logfname
                then
                    echo "      ${dbname}:initdata      OK";
                    echo "==============[INDEXES]==============" >> $logfname
                    if psql -d ${dbname} < $indexesfname &>> $logfname
                    then
                        echo "      ${dbname}:indexes       OK";
                        if cat $logfname | grep -A2 -n -T -i "ERROR:"
                        then
                            echo "      ${dbname}:done          ERROR";
                        else
                            echo "      ${dbname}:done          OK";
                        fi
                        echo "</${dbname}>";
                    else
                        echo "      ${dbname}:indexes       ERROR";
                        echo "[LOG]:"
                        tail $logfname;
                        echo "</${dbname}>";
                        exit 130;
                    fi
                else
                    echo "      ${dbname}:initdata      ERROR";
                    echo "[LOG]:";
                    tail $logfname;
                    echo "</${dbname}>";
                    exit 131;
                fi
            else
                echo "      ${dbname}:procedures    ERROR";
                echo "[LOG]:";
                tail $logfname;
                echo "</${dbname}>";
                exit 132;
            fi
        else
            echo "      ${dbname}:scheme        ERROR"
            echo "[LOG]:";
            tail $logfname;
            echo "</${dbname}>";
            exit 132;
        fi
    else
        echo "      ${dbname}:createdb      ERROR"
        echo "[LOG]:";
        tail $logfname;
        echo "</${dbname}>";
        exit 133;
    fi
}

##
## main() ->
##
    if dropdb ${dbname} &>> $logfname
    then
        ## Если база есть, то удаляем.
        echo "      ${dbname}:dropdb        OK";
        p_createdb;
    else
        ## Если базы нет, то просто создаем.
        p_createdb;
    fi
    exit 0;