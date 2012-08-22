/***********************************************************************
*
* \file Вставка данных в базу
*
***********************************************************************/

/****************************************************************************
    =====================================================================
                                ЯЗЫКИ
    =====================================================================
****************************************************************************/


insert into lang (alias, descr)
    values
        ('en_gb', 'english language'),
        ('ru_ru', 'russian language');

insert into trtype (alias, descr)
    values
        ('static',  'static translation'),
        ('dynamic', 'dynamic translation');

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'english',         'lang',     'name_ti','en_gb',
            (select name_ti from lang  where alias='en_gb'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'английский',         'lang',     'name_ti','en_gb',
            (select name_ti from lang  where alias='en_gb'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'russian',         'lang',     'name_ti','ru_ru',
            (select name_ti from lang  where alias='ru_ru'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'русский',         'lang',     'name_ti','ru_ru',
            (select name_ti from lang  where alias='ru_ru'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'static',      'trtype',       'name_ti','static',
            (select name_ti from trtype  where alias='static'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'статический',  'trtype',      'name_ti','static',
            (select name_ti from trtype  where alias='static'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'dynamic',      'trtype',      'name_ti','dynamic',
            (select name_ti from trtype  where alias='dynamic'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'динамический', 'trtype',      'name_ti','dynamic',
            (select name_ti from trtype  where alias='dynamic'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );



-- insert into tr (text, ti, lang_id, type_id)
--     values
--         (   'language',     -1,
--             (select id from lang where alias='en_gb'),
--             (select id from trtype where alias='static')
--         ),
--         (   'язык',         -1,
--             (select id from lang where alias='ru_ru'),
--             (select id from trtype where alias='static')
--         ),
--         (   'russian',      -2,
--             (select id from lang where alias='en_gb'),
--             (select id from trtype where alias='static')
--         ),
--         (   'русский',      -2,
--             (select id from lang where alias='ru_ru'),
--             (select id from trtype where alias='static')
--         ),
--         (   'english',      -3,
--             (select id from lang where alias='en_gb'),
--             (select id from trtype where alias='static')
--         ),
--         (   'английский',   -3,
--             (select id from lang where alias='ru_ru'),
--             (select id from trtype where alias='static')
--         );
-- 




/****************************************************************************
    =====================================================================
                                ФАЙЛЫ
    =====================================================================
****************************************************************************/




/****************************************************************************
    =====================================================================
                                ПОЛЬЗОВАТЕЛЬ
    =====================================================================
****************************************************************************/


insert into permtype (alias)
    values
        ('static');


insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'static',       'permtype',     'name_ti','static',
            (select name_ti from permtype where alias='static'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'статичные',     'permtype',     'name_ti','static',
            (select name_ti from permtype  where alias='static'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );



insert into permentitytype (alias)
    values
        ('pers');

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'pers',         'permentitytype',     'name_ti','pers',
            (select name_ti from permentitytype where alias='pers'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'перс',         'permentitytype',     'name_ti','pers',
            (select name_ti from permentitytype where alias='pers'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );


insert into perm (alias, permtype_id, entitytype_id)
    values
        ('system', 
            (select id from permtype where alias='static'),
            (select id from permentitytype where alias='pers')
        ),
        ('admin', 
            (select id from permtype where alias='static'),
            (select id from permentitytype where alias='pers')
        ),
        ('undel_pers', 
            (select id from permtype where alias='static'),
            (select id from permentitytype where alias='pers')
        ),
        ('undel_group',
            (select id from permtype where alias='static'),
            (select id from permentitytype where alias='pers')
        ),
        ('sysmsg', 
            (select id from permtype where alias='static'),
            (select id from permentitytype where alias='pers')
        ),
        ('sysconfiger', 
            (select id from permtype where alias='static'),
            (select id from permentitytype where alias='pers')
        ),
        ('contman', 
            (select id from permtype where alias='static'),
            (select id from permentitytype where alias='pers')
        );


insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'allowed only for the system itself',
            'perm','name_ti','system',
            (select name_ti from perm where alias='system'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'права разрешены только для самой системы',
            'perm','name_ti','system',
            (select name_ti from perm where alias='system'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'full access',
            'perm','name_ti','admin',
            (select name_ti from perm where alias='admin'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'полный доступ',
            'perm','name_ti','admin',
            (select name_ti from perm where alias='admin'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'can not be removed',
            'perm','name_ti','undel_pers',
            (select name_ti from perm where alias='undel_pers'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'нельзя удалить',
            'perm','name_ti','undel_pers',
            (select name_ti from perm where alias='undel_pers'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'can not be removed',
            'perm','name_ti','undel_group',
            (select name_ti from perm where alias='undel_group'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'нельзя удалить',
            'perm','name_ti','undel_group',
            (select name_ti from perm where alias='undel_group'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'to recieve system messages',
            'perm','name_ti','sysmsg',
            (select name_ti from perm where alias='sysmsg'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'получать системные сообщения',
            'perm','name_ti','sysmsg',
            (select name_ti from perm where alias='sysmsg'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'to config the system',
            'perm','name_ti','sysconfiger',
            (select name_ti from perm where alias='sysconfiger'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'конфигурировать систему',
            'perm','name_ti','sysconfiger',
            (select name_ti from perm where alias='sysconfiger'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'to manage the content',
            'perm','name_ti','contman',
            (select name_ti from perm where alias='contman'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'управлять содержимым',
            'perm','name_ti','contman',
            (select name_ti from perm where alias='contman'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );





insert into pgroup (alias)
    values
        ('admin'),
        ('sysmsg'),
        ('sysconfiger'),
        ('contman');

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'administrators',
            'pgroup','name_ti','admin',
            (select name_ti from pgroup where alias='admin'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'администраторы',
            'pgroup','name_ti','admin',
            (select name_ti from pgroup where alias='admin'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'system messages recievers',
            'pgroup','name_ti','sysmsg',
            (select name_ti from pgroup where alias='sysmsg'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'получатели системных сообщений',
            'pgroup','name_ti','sysmsg',
            (select name_ti from pgroup where alias='sysmsg'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'system configer',
            'pgroup','name_ti','sysconfiger',
            (select name_ti from pgroup where alias='sysconfiger'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'системный конфигуратор',
            'pgroup','name_ti','sysconfiger',
            (select name_ti from pgroup where alias='sysconfiger'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'content manager',
            'pgroup','name_ti','contman',
            (select name_ti from pgroup where alias='contman'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'управляющий содержимым',
            'pgroup','name_ti','contman',
            (select name_ti from pgroup where alias='contman'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );


insert into perm2pgroup (perm_id, group_id)
    values
        ( (select id from perm where alias='admin'),
            (select id from pgroup where alias='admin')),
        ( (select id from perm where alias='undel_group'),
            (select id from pgroup where alias='admin')),
        ( (select id from perm where alias='undel_pers'),
            (select id from pgroup where alias='admin')),
        ( (select id from perm where alias='sysmsg'),
            (select id from pgroup where alias='admin')),
        ( (select id from perm where alias='contman'),
            (select id from pgroup where alias='admin')),
        ( (select id from perm where alias='undel_group'),
            (select id from pgroup where alias='sysmsg')),
        ( (select id from perm where alias='sysmsg'),
            (select id from pgroup where alias='sysmsg')),
        ( (select id from perm where alias='contman'),
            (select id from pgroup where alias='sysmsg')),
        ( (select id from perm where alias='undel_group'),
            (select id from pgroup where alias='contman')),
        ( (select id from perm where alias='contman'),
            (select id from pgroup where alias='contman'));


insert into pers (fname, sname, email, phone, nick, login, phash)
    values ('fadmin', 'sadmin', 'padmin@padmin.ru',
            293203230230, 'admin', 'admin', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (fname, sname, email, phone, nick, login, phash)
    values ('fname', 'sname', 'email@email.ru',
            293203230230, 'nick', 'login', '21232F297A57A5A743894A0E4A801FC3');

    --//
    --     admin -> 21232F297A57A5A743894A0E4A801FC3
    --// новыйпароль
    --     yjdsqgfhjkm -> C7BCC36975D86BB977D99A7DFB8EBDA0
    --//
    --     c7bcc36975d86bb977d99a7dfb8ebda0 -> D28847DA5504EE1365C159BC8FA18198
    --// md5sum этого файла
    --     ac4e05cfe177d66bf630dab627e81ab0 -> 344F01D45FFCE96499C7B8966BD176E0
    --//
    --     mjkqgjdsyhf -> C72DC633185E52C9FD363EEED7220C85
    --//
    --     etsuken -> C61B248A4D509E2923EBD983A8658C55


insert into pers2pgroup (pers_id, group_id)
    values
        ((select id from pers where nick='admin'),
            (select id from pgroup where alias='admin')),
        ((select id from pers where nick='admin'),
            (select id from pgroup where alias='sysmsg')),
        ((select id from pers where nick='admin'),
            (select id from pgroup where alias='contman'));

insert into emotion(alias)
    values ('happy'), ('indifferent'), ('sad');

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'happy',        'emotion',      'name_ti','happy',
            (select name_ti from emotion  where alias='happy'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'indifferent',  'emotion',      'name_ti','indifferent',
            (select name_ti from emotion  where alias='indifferent'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'sad',          'emotion',      'name_ti','sad',
            (select name_ti from emotion  where alias='sad'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'радуется',     'emotion',      'name_ti','happy',
            (select name_ti from emotion  where alias='happy'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'равнодушен',   'emotion',      'name_ti','indifferent',
            (select name_ti from emotion  where alias='indifferent'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'грустный',     'emotion',      'name_ti','sad',
            (select name_ti from emotion  where alias='sad'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );

insert into authority(alias, level)
    values
        ('troll',       -100),
        ('bully',       -50),
        ('noob',        0),
        ('inhabitant',  50),
        ('citizen',     100),
        ('elder',       150);

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'troll',        'authority',      'name_ti','troll',
            (select name_ti from authority  where alias='troll'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'bully',        'authority',      'name_ti','bully',
            (select name_ti from authority  where alias='bully'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'noob',         'authority',      'name_ti','noob',
            (select name_ti from authority  where alias='noob'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'inhabitant',   'authority',      'name_ti','inhabitant',
            (select name_ti from authority  where alias='inhabitant'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'citizen',      'authority',      'name_ti','citizen',
            (select name_ti from authority  where alias='citizen'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'elder',        'authority',      'name_ti','elder',
            (select name_ti from authority  where alias='elder'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'троль',        'authority',      'name_ti','troll',
            (select name_ti from authority  where alias='troll'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'хулиган',      'authority',      'name_ti','bully',
            (select name_ti from authority  where alias='bully'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'новичок',      'authority',      'name_ti','noob',
            (select name_ti from authority  where alias='noob'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'житель',       'authority',      'name_ti','inhabitant',
            (select name_ti from authority  where alias='inhabitant'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'гражданин',    'authority',      'name_ti','citizen',
            (select name_ti from authority  where alias='citizen'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'старейшина',   'authority',      'name_ti','elder',
            (select name_ti from authority  where alias='elder'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );


insert into pstatus(alias)
    values ('online'),('offline'),('banned'),('killed');

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'online',       'pstatus',      'name_ti','online',
            (select name_ti from pstatus  where alias='online'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'offline',      'pstatus',      'name_ti','offline', 
            (select name_ti from pstatus  where alias='offline'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'banned',       'pstatus',      'name_ti','banned', 
            (select name_ti from pstatus  where alias='banned'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'killed',       'pstatus',      'name_ti','killed', 
            (select name_ti from pstatus  where alias='killed'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'в сети',       'pstatus',      'name_ti','online', 
            (select name_ti from pstatus  where alias='online'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'не в сети',    'pstatus',      'name_ti','offline', 
            (select name_ti from pstatus  where alias='offline'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'забанен',      'pstatus',      'name_ti','banned', 
            (select name_ti from pstatus  where alias='banned'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'убит',         'pstatus',      'name_ti','killed', 
            (select name_ti from pstatus  where alias='killed'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );



insert into mstatus(alias)
    values ('single'),('engaged'),('married'),('divorced');

insert into tr (text, tt, tf, ta, ti,  lang_id, type_id)
    values
        (   'single',       'mstatus',      'name_ti','single',
            (select name_ti from mstatus  where alias='single'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'engaged',      'mstatus',      'name_ti','engaged', 
            (select name_ti from mstatus  where alias='engaged'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'married',      'mstatus',      'name_ti','married',
            (select name_ti from mstatus  where alias='married'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'divorced',     'mstatus',      'name_ti','divorced',
            (select name_ti from mstatus  where alias='divorced'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'одиночество', 'mstatus',       'name_ti','single',
            (select name_ti from mstatus  where alias='single'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'помовлка',     'mstatus',      'name_ti','engaged', 
            (select name_ti from mstatus  where alias='engaged'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'брак',         'mstatus',      'name_ti','married',
            (select name_ti from mstatus  where alias='married'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'развод',       'mstatus',      'name_ti','divorced',
            (select name_ti from mstatus  where alias='divorced'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );

-------------------------------------------------------------------------------
-- Документы
-------------------------------------------------------------------------------

insert into acctype (alias)
    values ('private'), ('protected'), ('public');

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'private',      'acctype',      'name_ti','private',
            (select name_ti from acctype  where alias='private'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'protected',    'acctype',      'name_ti','protected', 
            (select name_ti from acctype  where alias='protected'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'public',       'acctype',      'name_ti','public', 
            (select name_ti from acctype  where alias='public'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'личный',       'acctype',      'name_ti','private', 
            (select name_ti from acctype  where alias='private'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'защищенный',   'acctype',      'name_ti','protected', 
            (select name_ti from acctype  where alias='protected'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'публичный',    'acctype',      'name_ti','public',
            (select name_ti from acctype  where alias='public'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );

insert into contype(alias)
    values ('common'), ('adult_only');

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'common',       'contype',     'name_ti','common',
            (select name_ti from contype where alias='common'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'adult only',   'contype',     'name_ti','adult only',
            (select name_ti from contype where alias='adult_only'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'обычный',      'contype',      'name_ti','common',
            (select name_ti from contype  where alias='common'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'эротический',  'contype',      'name_ti','adult only',
            (select name_ti from contype  where alias='adult_only'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );


insert into doctype(alias)
    values ('blog'), ('post'), ('gallery'), ('photo'), ('attach_descr');

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'blog',         'doctype',     'name_ti','blog',
            (select name_ti from doctype where alias='blog'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'блог',         'doctype',     'name_ti','blog',
            (select name_ti from doctype where alias='blog'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'post',         'doctype',     'name_ti','post',
            (select name_ti from doctype where alias='post'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'пост',         'doctype',     'name_ti','post',
            (select name_ti from doctype where alias='post'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'gallery',      'doctype',     'name_ti','gallery',
            (select name_ti from doctype where alias='gallery'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'галерея',      'doctype',     'name_ti','gallery',
            (select name_ti from doctype where alias='gallery'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'photo',        'doctype',     'name_ti','photo',
            (select name_ti from doctype where alias='photo'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'фото',         'doctype',     'name_ti','photo',
            (select name_ti from doctype where alias='photo'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'attach',       'doctype',     'name_ti','attach',
            (select name_ti from doctype where alias='attach_descr'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'приложение',   'doctype',     'name_ti','attach',
            (select name_ti from doctype where alias='attach_descr'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );

insert into roomtype(alias)
    values ('lang'), ('prison'), ('hell'), ('heaven');

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'land',         'roomtype',     'name_ti','lang',
            (select name_ti from roomtype where alias='lang'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'страна',       'roomtype',     'name_ti','lang',
            (select name_ti from roomtype where alias='lang'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'prison',       'roomtype',     'name_ti','prison',
            (select name_ti from roomtype where alias='prison'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'тюрьма',       'roomtype',     'name_ti','prison',
            (select name_ti from roomtype where alias='prison'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'hell',         'roomtype',     'name_ti','hell',
            (select name_ti from roomtype where alias='hell'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'ад',           'roomtype',     'name_ti','hell',
            (select name_ti from roomtype where alias='hell'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'heaven',       'roomtype',     'name_ti','heaven',
            (select name_ti from roomtype where alias='heaven'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'рай',          'roomtype',     'name_ti','heaven',
            (select name_ti from roomtype where alias='heaven'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );

insert into chatlang(alias)
    values
        ('en_gb'), -- английский
        ('ar_ar'), -- арабский
        ('sp_sp'), -- испанский
        ('ch_ch'), -- китайский
        ('ru_ru'), -- русский 
        ('fr_fr') -- французский
        ;

insert into tr (text, tt, tf, ta, ti, lang_id, type_id)
    values
        (   'english',      'chatlang',     'name_ti','en_gb',
            (select name_ti from chatlang where alias='en_gb'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'английский',   'chatlang',     'name_ti','en_gb',
            (select name_ti from chatlang where alias='en_gb'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'arabic',       'chatlang',     'name_ti','ar_ar',
            (select name_ti from chatlang where alias='ar_ar'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'арабский',     'chatlang',     'name_ti','ar_ar',
            (select name_ti from chatlang where alias='ar_ar'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'spanish',      'chatlang',     'name_ti','sp_sp',
            (select name_ti from chatlang where alias='sp_sp'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'испанский',    'chatlang',     'name_ti','sp_sp',
            (select name_ti from chatlang where alias='sp_sp'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'chinese',      'chatlang',     'name_ti','ch_ch',
            (select name_ti from chatlang where alias='ch_ch'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'китайский',    'chatlang',     'name_ti','ch_ch',
            (select name_ti from chatlang where alias='ch_ch'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'russian',      'chatlang',     'name_ti','ru_ru',
            (select name_ti from chatlang where alias='ru_ru'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'русский',      'chatlang',     'name_ti','ru_ru',
            (select name_ti from chatlang where alias='ru_ru'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'french',       'chatlang',     'name_ti','fr_fr',
            (select name_ti from chatlang where alias='fr_fr'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'французский',  'chatlang',     'name_ti','fr_fr',
            (select name_ti from chatlang where alias='fr_fr'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );
