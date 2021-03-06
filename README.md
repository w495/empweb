# ЧТО ЭТО?

Серверная часть проекта `Empweb`.

# CTРУКТУРА ПРОЕКТА
    .
    ├── empweb/
    │   │
    │   ├── deps/           ВСПОМОГАТЕЛЬНЫЕ ПРИЛОЖЕНИЯ И МОДУЛИ.
    │   │   ├── amnesia/    Обертка для `mnesia`, нужна для веб сессий.
    │   │   ├── cowboy/     Http-аксептор. Основной http-сервер.
    │   │   ├── ejson/      Драйвер для `json` (на `С`).
    │   │   ├── empdb/      Логика работы с базой данных.
    │   │   ├── epgsql/     Драйвер для Postgresql.
    │   │   ├── erlxslt/    Драйвер для `xslt` (на `С`) ['НЕ ИСПОЛЬЗУЕТСЯ'].
    │   │   ├── evman/      Менеджер событий (пока толбко для логирования).
    │   │   ├── gen_smtp/   Smtp-клиент.
    │   │   ├── gm/         Порты для `GraphicsMagick`.
    │   │   ├── lager/      Сервер логирование.
    │   │   ├── lgps/       Приложение генерации последовательностей мнемоник.
    │   │   ├── mochiweb/   Альтернативный сервер `http`, запаска для `ejson`.
    │   │   ├── nodeclt/    Приложение управление для нодой.
    │   │   ├── norm/       Нормировка входных параметров.
    │   │   ├── poolboy/    Универсальный пулл чего-либо.
    │   │   ├── psqlcp/     Пулл соединений с `Postgresql` через `epgsql`.
    │   │   ├── term_cache/ Сервер кеширования в памяти (на `ets`),
    │   │   └── README.md   Минимальное описание вспомогательных модулей.
    │   │
    │   ├── include/                    ЗАГОЛОВКИ ОСНОВНОГО ПРИЛОЖЕНИЯ.
    │   │   ├── empweb_biz_session.hrl  Описание структуры сессии.
    │   │   └── empweb.hrl              Структуры и констатнты.
    │   │
    │   ├── priv/               ВСПОМОГАТЕЛЬНЫЕ ФАЙЛЫ ОСНОВНОГО ПРИЛОЖЕНИЯ.
    │   │   ├── logs/           Логи от `lager`.
    │   │   ├── session-db/     Файлы сессий.
    │   │   ├── ssl/            Ключи для ssl и https.
    │   │   ├── static/         Вспомогательные статичесие фалы, ресурсы.
    │   │   └── xsl/            Шаблоны xsl ['НЕ ИСПОЛЬЗУЕТСЯ'].
    │   │
    │   ├── src/                            ИСХОДНИКИ ОСНОВНОГО ПРИЛОЖЕНИЯ.
    │   │   ├── biz/                        Папка с бизнес логикой.
    │   │   ├── handler/                    Обработчики внешних запросов.
    │   │   ├── jsonapi/                    Внутренние обработчики (нормируют
    │   │   │                               и вызывают функции из biz/).
    │   │   ├── norm/                       Нормировка.
    │   │   ├── empweb_app.erl              Модуль ОТП-приложения.
    │   │   ├── empweb.app.src              Описание ОТП-приложения.
    │   │   ├── empweb_convert.erl          Модуль конвертации.
    │   │   ├── empweb_dispatcher.erl       Статический диспетчер `URL`.
    │   │   ├── empweb.erl                  Основной модуль приложения
    │   │   ├── empweb_http_controller.erl  Интерфейс контроллера `http`.
    │   │   ├── empweb_http.erl             Общие функции для работы с `http`.
    │   │   ├── empweb_http_hap.erl         Интерфейс внутреннего контроллера.
    │   │   ├── empweb_jsonapi.erl          Общие функции для работы с `json`.
    │   │   ├── empweb_mailutils.erl        Общие функции отправки почты.
    │   │   ├── empweb_norm.erl             Общие функции нормировки.
    │   │   ├── empweb_sup.erl              Главный наблюдатель приложения.
    │   │   └── empweb_uuid.erl             Генератор `GUID`.
    │   │
    │   ├── ctl.sh*             Запускающий скрипт.
    │   ├── ctl-dev.sh          Запускающий скрипт для локальной отладки.
    │   ├── empweb.config       Конфигурация приложения.
    │   ├── empweb-dev.config   Конфигурация приложения для локальной отладки.
    │   ├── Makefile            Инструкция для сборки через `make`.
    │   ├── r15b.sh*            Управление путями для локального запуска.
    │   ├── rebar*              Менеджер сборки.
    │   └── rebar.config        Конфигурация сборки.
    │
    ├── CHANGELOG.md            Список самых глобальных изменений.
    ├── INSTALL.md              Инструкция по установке.
    └── README.md               Описание проекта


