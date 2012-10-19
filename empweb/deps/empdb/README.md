# Что это?

Интерфейс для работы с базой данных и бизнесс логика уровня данных
для проекта Empire.

# Что Нужно?

Для нормальной работы нужны
    *   psqlcp --- как пулл соединения с СУБД  кеширPostgres;
    *   term_cache --- как инструмент кеширования.


# Как это испольовать

Запускать приложение empdb можно как отдельную OTP-ноду.
Для этого


    Возможно повторное использование некоторых компонент.
    Например, очень хорошо, было бы вынести молуль empdb_dao
    и связанные с ним в отдельное OTP-приложение.

    
# TODO
    *   Коментировать код;
    *   Разделить некоторые модули по моделям (таблицам базы данных);
    *   Вынести молуль empdb_dao и связанные с ним в отдельное OTP-приложение.

# Структура

    ├── CHANGELOG.md
    ├── ctl.sh
    ├── ebin
    ├── empdb.config
    ├── include
    │   └── empdb.hrl
    ├── Makefile
    ├── priv
    │   ├── old
    │   │   ├── empdb_biz_doc.erl.effective_nposts
    │   │   ├── empdb_biz_post.erl.1
    │   │   ├── empdb_dao_doc.erl.1
    │   │   ├── empdb_dao.erl.1
    │   │   ├── empdb_memo.erl.1
    │   │   ├── empdb_storage.erl.1
    │   │   ├── empdb_utils.erl.1
    │   │   ├── proc.for_future.sql
    │   │   └── proc.sql.1
    │   └── sql
    │       ├── db_change.sql
    │       ├── ejabberd.sql
    │       ├── geo.sql
    │       ├── indexes.sql
    │       ├── insert.sql
    │       ├── log.log
    │       ├── mkdump.sh
    │       ├── proc.sql
    │       ├── redb.sh
    │       └── scheme.sql
    ├── README.md
    ├── rebar
    ├── rebar.config
    └── src
        ├── biz
        │   ├── empdb_biz_doc.erl
        │   ├── empdb_biz_experbuy.erl
        │   ├── empdb_biz_lang.erl
        │   ├── empdb_biz_pers.erl
        │   ├── empdb_biz_room.erl
        │   ├── empdb_biz_thingbuy.erl
        │   ├── empdb_biz_thing.erl
        │   └── empdb_biz_thingtype.erl
        ├── dao
        │   ├── empdb_dao_blog.erl
        │   ├── empdb_dao_comment.erl
        │   ├── empdb_dao_community.erl
        │   ├── empdb_dao_doc.erl
        │   ├── empdb_dao_event.erl
        │   ├── empdb_dao_experbuy.erl
        │   ├── empdb_dao_lang.erl
        │   ├── empdb_dao_message.erl
        │   ├── empdb_dao_pers.erl
        │   ├── empdb_dao_post.erl
        │   ├── empdb_dao_room.erl
        │   ├── empdb_dao_sysvar.erl
        │   ├── empdb_dao_thingbuy.erl
        │   ├── empdb_dao_thing.erl
        │   ├── empdb_dao_thingtype.erl
        │   └── empdb_dao_tr.erl
        ├── empdb_app.erl
        ├── empdb.app.src
        ├── empdb_convert.erl
        ├── empdb_dao.erl
        ├── empdb.erl
        └── empdb_sup.erl

    8 directories, 57 files
