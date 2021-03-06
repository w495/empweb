# ЧТО НУЖНО

1. `git`.
2. gnu `make`.
3. `gcc` & `g++`.
4. `libxml2` & `libxslt`.
5. `erlang`. Нужен R14 или R15, c R16 работать не будет.
6. `GraphicsMagick` Нужна утилита `gm` (для конвертации фоток).
7. `Postgresql`. После ее установки прописать `trust`
в `/var/lib/pgsql/data/pg_hba.conf`.
8. `ejabberd`.

# УСТАНОВКА

1. Установить и настроить `ejabberd`.
2. Настроить базы данных.
3. Перенести файлы пользователей, если требуется.
4. Собрать проект через `make`.
5. Настроить проект.

## EJABBERD

Установить и настроить `ejabberd` стандарнтым образом, так чтобы он использова
СУБД `Postgresql`. База данных его должна называться `ejabberd`.

## БАЗЫ ДАННЫХ

### УСТАНОВКА ПУСТОЙ БАЗЫ

Запустить скрипт начальной конфигурации баз
`./empweb/deps/empdb/priv/sql/create/redb2.sh`
Будет создана база данных `emp`.
При прочих раных условиях лучше пользоваться готовыми дампами.
Скрипты создания схемы давно не проверялись на актуальнсоть.

### МИГРАЦИЯ НА НОВЫЙ СЕРВЕР

1. Сделать дамп текущей развернутой версии версии.
`./empweb/deps/empdb/priv/sql/dumps/mkdump.sh`
Утилита создаст дампы двух бд
* `ejabberd`;
* `emp`.
2. Создать базы на новом сервере:
* `ejabberd`;
* `emp`.

    $ sudo su postgres
    >$ createuser  root -s -r -P
    Enter password for new role: root
    Enter it again: root
    $ cd ./empweb/deps/empdb/priv/sql/dumps/
    $ createdb emp
    $ psql -d emp < dump-emp-<date>.sql
    $ createdb ejabberd
    $ psql -d emp < dump-ejabberd-<date>.sql

3. Применить дампы созданные через `mkdump.sh` к созданным базам.

## ФАЙЛЫ

### УСТАНОВКА ПУСТОЙ БАЗЫ

Ничего делать не нужно.

### МИГРАЦИЯ НА НОВЫЙ СЕРВЕР

Перенести папку  `./empweb/deps/empdb/priv/data/` на новое место установки.

## СБОРКА ПРОЕКТА

Набрать команды: `cd ./empweb && make`.
Если `erlang` или `rebar` запросят какие-то пакеты просто установить
их стандартным способом (через менеджер пакетов).

## НАСТРОЙКА ПРОЕКТА

Отредактировать `./empweb/empweb.config` согласно настройкам сервера и СУБД.
Крайне желательно оставлять эти настройки неизменными.

# ЗАПУСК ПРОЕКТА

## ТЕСТОВЫЙ

* `cd ./empweb`.
* `./ctl.sh start`.

## ФОНОВЫЙ (БОЕВОЙ)

* `cd ./empweb`.
* `./ctl.sh startd`.

## ФОНОВЫЙ (БОЕВОЙ)

* `cd ./empweb`.
* `./ctl.sh startd`.

### ПАРАМЕТРЫ CTL.SH

* `start [-detached]`   — запусткает ноду;
* `startd`              — запусткает ноду в фоне, псевдоним к `start -detached`;
* `reload_code`         — заменяет код;
* `reload_cfg`          — заменяет конфигурацию ноды;
* `status`              — возвращает статус приложения;
* `stop`                — останавливает приложение и ноду;
* `stop_app`            — останавливает только приложение;
* `start_app`           — запускает приложение поверх запущенной ноды;
* `version`             — возвращает версию приложения.

При текущей конфигурации обновление кода происходит по его пересборке.
Внутри `./ctl.sh`. Для горячей замене по `make`:

    erl \
        -pa $BINPATH \
        -boot start_sasl \
        -config ${CONFIG} \
        -name ${MAIN_NODE} \
        -setcookie ${COOKIE} \
        -s nodeclt_reloader \
        -s ${MAIN_APP} \
        -mnesia dir $SESSIONDBPATH \
        ${ERL_ARGS} \
    "$@"

Для горячей замене по `./ctl.sh reload_code`

    erl \
        -noinput \
        -pa $BINPATH \
        -name ${CTRL_NODE} \
        -setcookie ${COOKIE} \
        -s nodeclt \
        -hidden \
        -connect_all false \
        ${ERL_ARGS} \
        -extra -n ${MAIN_NODE} \
    "$@"

# КОНТАКТЫ НА СЛУЧАЙ ПРОБЛЕМ

* почта:      nikitin.i@tvzavr.ru
* почта:      w@w-495.ru
* twitter:    @w_495
* skype:      w-495-nb
* jabber:     w-495@jabber.ru
* телефон:    +7  499 196 68 58
* телефон:    +7  499 196 49 58
* телефон:    +7  916 536-05-41
* icq:                531-04-41


