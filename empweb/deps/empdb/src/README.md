# ЧТО ЭТО?

Исходные коды интерфейса для работы с базой данных
и бизнес логики уровня данных для проекта `Empire`.


# СТРУКТУРА КАТАЛОГОВ
    .
    ├──[biz]                                # БИЗНЕС-ЛОГИКА;
    │   ├── empdb_biz_action.erl            # действия пользователя;
    │   ├── empdb_biz_actiontype.erl        # действия пользователя;
    │   ├── empdb_biz_album.erl
    │   ├── empdb_biz_arms.erl
    │   ├── empdb_biz_attach.erl
    │   ├── empdb_biz_attachtype.erl
    │   ├── empdb_biz_back.erl
    │   ├── empdb_biz_claim.erl
    │   ├── empdb_biz_claimtype.erl
    │   ├── empdb_biz_communitycand.erl
    │   ├── empdb_biz_community.erl
    │   ├── empdb_biz_communityhist.erl
    │   ├── empdb_biz_communityhisttype.erl
    │   ├── empdb_biz_communitymemb.erl
    │   ├── empdb_biz_communitytreas.erl
    │   ├── empdb_biz_cptrans.erl
    │   ├── empdb_biz_doc.erl
    │   ├── empdb_biz_eventact.erl
    │   ├── empdb_biz_event.erl
    │   ├── empdb_biz_eventobj.erl
    │   ├── empdb_biz_eventspc.erl
    │   ├── empdb_biz_eventtype.erl
    │   ├── empdb_biz_exile.erl
    │   ├── empdb_biz_experbuy.erl
    │   ├── empdb_biz_file.erl
    │   ├── empdb_biz_firecounts.erl
    │   ├── empdb_biz_flag.erl
    │   ├── empdb_biz_friendtype.erl
    │   ├── empdb_biz_geo.erl
    │   ├── empdb_biz_invisbuy.erl
    │   ├── empdb_biz_invistype.erl
    │   ├── empdb_biz_lang.erl
    │   ├── empdb_biz_notice.erl
    │   ├── empdb_biz_noticetype.erl
    │   ├── empdb_biz_pay.erl
    │   ├── empdb_biz_paytype.erl
    │   ├── empdb_biz_pers.erl
    │   ├── empdb_biz_perspicbody.erl
    │   ├── empdb_biz_perspichead.erl
    │   ├── empdb_biz_photo.erl
    │   ├── empdb_biz_repost.erl
    │   ├── empdb_biz_roombet.erl
    │   ├── empdb_biz_room.erl
    │   ├── empdb_biz_roomexperbuy.erl
    │   ├── empdb_biz_roomlist.erl
    │   ├── empdb_biz_roomlisttype.erl
    │   ├── empdb_biz_roomlot.erl
    │   ├── empdb_biz_roomtreas.erl
    │   ├── empdb_biz_rptrans.erl
    │   ├── empdb_biz_service.erl
    │   ├── empdb_biz_thingbuy.erl
    │   ├── empdb_biz_thing.erl
    │   ├── empdb_biz_thingtype.erl
    │   ├── empdb_biz_thingwish.erl
    │   ├── empdb_biz_transtype.erl
    │   ├── empdb_biz_treastype.erl
    │   ├── empdb_biz_vote.erl
    │   ├── empdb_biz_wall.erl
    │   └── empdb_biz_zprotbuy.erl
    │
    ├──[dao]                # обертка логики БД;
    ├──[daowp]              # взаимодействие с БД;
    ├── empdb.app.src       # файл ресурсов приложения;
    ├── empdb_app.erl       # модуль, стартующий приложение;
    ├── empdb_base62.erl    # кодирование в `base62` [НЕ ИСПОЛЬЗУЕТСЯ!];
    ├── empdb_biz.erl       # общие функции бизнес-логики,
    │                       #   `gen_server` используется как таймер;
    ├── empdb_convert.erl   # функции конвертации различных типов;
    ├── empdb_dao.erl       # общие функции взаимодействия с БД,
    │                       #   содержит реализация простейшей ORM;
    ├── empdb.erl           # пустой модуль, [НЕ ИСПОЛЬЗУЕТСЯ!];
    ├── empdb_orm_util.erl  # вспомогательный модуль для `empdb_dao.erl`
    │                       #   предполагается, что часть функция будут
    │                       #   перенесены сюда;
    ├── empdb_suggest.erl   # модуль генерации "похожих" строк,
    │                       #   используется для предложения
    │                       #   новых имен пользователей.
    ├── empdb_sup.erl       # модуль наблюдения;
    ├── empdb_timer.erl     # таймер, различные действия раз в какое-то время,
    │                       #   может быть заменен стандартными функциями
    │                       #   `timer:apply_interval/4`;
    ├── empdb_uuid.erl      # модуль генерации uuid.
    └── README.md           # описание исходников приложения.
