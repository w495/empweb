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

select 'log:lang & log:trtype' as log;

insert into lang (alias, descr)
    values
        ('en_gb', 'english language'),
        ('ru_ru', 'russian language');

insert into trtype (alias, descr)
    values
        ('static',  'static translation'),
        ('dynamic', 'dynamic translation');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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

select 'log:permtype' as log;

insert into permtype (alias)
    values
        ('static');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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

select 'log:permentitytype' as log;

insert into permentitytype (alias)
    values
        ('pers');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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

select 'log:perm' as log;

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

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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

select 'log:pgroup' as log;

insert into pgroup (alias)
    values
        ('admin'),
        ('sysmsg'),
        ('sysconfiger'),
        ('contman');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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

select 'log:perm2pgroup' as log;

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


select 'log:emotion' as log;

insert into emotion(alias)
    values ('happy'), ('indifferent'), ('sad');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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

select 'log:authority' as log;

insert into authority(alias, level)
    values
        ('troll',       -100),
        ('bully',       -50),
        ('noob',        0),
        ('inhabitant',  50),
        ('citizen',     100),
        ('elder',       150);

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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

select 'log:pstatus' as log;

insert into pstatus(alias)
    values ('online'),('offline'),('banned'),('killed');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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

select 'log:mstatus' as log;

insert into mstatus(alias)
    values ('single'),('engaged'),('married'),('divorced');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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

/****************************************************************************
    =====================================================================
                                НАСТРОЙКИ
    =====================================================================
****************************************************************************/

select 'log:sysvartype' as log;

insert into sysvartype (alias)
    values  ('text'), ('int'), ('bool'), ('real'), ('void');


insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'text',         'sysvartype',      'name_ti','text',
            (select name_ti from sysvartype  where alias='text'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'integer',      'sysvartype',      'name_ti','int',
            (select name_ti from sysvartype  where alias='int'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'boolean',      'sysvartype',      'name_ti','bool',
            (select name_ti from sysvartype  where alias='bool'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'real',         'sysvartype',      'name_ti','real',
            (select name_ti from sysvartype  where alias='real'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'void',         'sysvartype',      'name_ti','void',
            (select name_ti from sysvartype  where alias='void'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'текст',        'sysvartype',      'name_ti','text',
            (select name_ti from sysvartype  where alias='text'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'целое',        'sysvartype',      'name_ti','int',
            (select name_ti from sysvartype  where alias='int'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'логическое',   'sysvartype',      'name_ti','bool',
            (select name_ti from sysvartype  where alias='bool'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'вещественное', 'sysvartype',      'name_ti','real',
            (select name_ti from sysvartype  where alias='real'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'пустое',       'sysvartype',      'name_ti','void',
            (select name_ti from sysvartype  where alias='void'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );


-----------------------------------------------------------------------
-- Документы
-----------------------------------------------------------------------

select 'log:oktype' as log;

insert into oktype (alias)
    values ('ncons'), ('forbidden'), ('ok');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'not considered',   'oktype',  'name_ti','ncons',
            (select name_ti from oktype  where alias='ncons'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'forbidden',    'oktype',      'name_ti','forbidden',
            (select name_ti from oktype  where alias='forbidden'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'ok',           'oktype',      'name_ti','ok',
            (select name_ti from oktype  where alias='ok'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'не просмотрен',    'oktype',  'name_ti','ncons',
            (select name_ti from oktype  where alias='ncons'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'запрещено',   'oktype',       'name_ti','forbidden',
            (select name_ti from oktype  where alias='forbidden'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'разрешено',    'oktype',      'name_ti','ok',
            (select name_ti from oktype  where alias='ok'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );

select 'log:acctype' as log;

insert into acctype (alias)
    values ('private'), ('protected'), ('public');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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

select 'log:contype' as log;

insert into contype(alias)
    values ('common'), ('adult_only');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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


select 'log:doctype' as log;

insert into doctype(alias)
    values  ('blog'),
            ('post'),
            ('comment'),
            ('attach'),
            ('room'),
            ('community'),
            ('message'),
            ('notice'),
            ('event'),
            ('roomlot'),
            ('album'),
            ('photo'),
            ('claim');
            
insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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
        (   'comment',      'doctype',     'name_ti','comment',
            (select name_ti from doctype where alias='comment'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'комментарий',  'doctype',     'name_ti','comment',
            (select name_ti from doctype where alias='comment'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'attach',       'doctype',     'name_ti','attach',
            (select name_ti from doctype where alias='attach'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'вложение',     'doctype',     'name_ti','attach',
            (select name_ti from doctype where alias='attach'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        
        (   'room',         'doctype',     'name_ti','room',
            (select name_ti from doctype where alias='room'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'комната',      'doctype',     'name_ti','room',
            (select name_ti from doctype where alias='room'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),

        (   'community',    'doctype',     'name_ti','community',
            (select name_ti from doctype where alias='community'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'cooбщество',      'doctype',     'name_ti','community',
            (select name_ti from doctype where alias='community'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        
        (   'message',      'doctype',     'name_ti','message',
            (select name_ti from doctype where alias='message'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'сообщение',      'doctype',   'name_ti','message',
            (select name_ti from doctype where alias='message'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'event',        'doctype',     'name_ti','event',
            (select name_ti from doctype where alias='event'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'событие',      'doctype',   'name_ti','event',
            (select name_ti from doctype where alias='event'),
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

select 'log:roomtype' as log;

insert into roomtype(alias)
    values ('land'), ('prison'), ('hell'), ('heaven'), ('noobs');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'land',         'roomtype',     'name_ti','land',
            (select name_ti from roomtype where alias='land'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'страна',       'roomtype',     'name_ti','land',
            (select name_ti from roomtype where alias='land'),
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
        ),
        (   'noobs',       'roomtype',      'name_ti','noobs',
            (select name_ti from roomtype where alias='noobs'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'новички',      'roomtype',     'name_ti','noobs',
            (select name_ti from roomtype where alias='noobs'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );

select 'log:chatlang' as log;

insert into chatlang(alias)
    values
        ('en_gb'), -- английский
        ('ar_ar'), -- арабский
        ('sp_sp'), -- испанский
        ('ch_ch'), -- китайский
        ('ru_ru'), -- русский 
        ('fr_fr') -- французский
        ;

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
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



select 'log:regimen' as log;

insert into regimen(alias)
    values
        ('despotism'), 
        ('monarchy'), 
        ('sultanate'),
        ('republic'), 
        ('anarchy'), 
        ('kingdom'),
        ('shogunate'),
        ('principality'),
        ('feudal'),
        ('tribe');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'despotism',
            'regimen', 'name_ti', 'despotism',
            (select name_ti from regimen where alias='despotism'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),(
            'The founder of the country is a life of its owner, on the one hand, it is tempting to create this type of country, on the other hand — in a country, other users will be severely limited in their rights compared to a dictator, so is unlikely to be many who want to visit a country.',
            'regimen', 'descr_ti', 'despotism',
            (select descr_ti from regimen where alias='despotism'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'monarchy',
            'regimen', 'name_ti', 'monarchy',
            (select name_ti from regimen where alias='monarchy'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ), (
            'Founder of the country became its first monarch, over time, may be a change of government (coup) — Crown can go to another, but only to someone who has become quite influential in the country, ie the country has some real merit.',
            'regimen', 'descr_ti', 'monarchy',
            (select descr_ti from regimen where alias='monarchy'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'sultanate',
            'regimen', 'name_ti', 'sultanate',
            (select name_ti from regimen where alias='sultanate'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ), (
            'Eastern monarchy. Founder of the country is a life of its owner, on the one hand, it is tempting to create this type of country, on the other hand — in a country, other users will be severely limited in their rights compared to a dictator, so is unlikely to be many who want to visit a country .',
            'regimen', 'descr_ti', 'sultanate',
            (select descr_ti from regimen where alias='sultanate'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'republic',
            'regimen', 'name_ti', 'republic',
            (select name_ti from regimen where alias='republic'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),(
            'Elected president. President is open to anyone, regardless of the authority (even a novice) and real merit, most importantly — popularity, chosen by voting for a certain period.',
            'regimen', 'descr_ti', 'republic',
            (select descr_ti from regimen where alias='republic'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'anarchy',
            'regimen', 'name_ti', 'anarchy',
            (select name_ti from regimen where alias='anarchy'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),(
            'The founder of the country (the owner) is no different from other people, except for the right to sell the country, bought her just will not have any privileges, except the right to resell the country. Accordingly, the people of the country no matter who the owner at the moment.',
            'regimen', 'descr_ti', 'anarchy',
            (select descr_ti from regimen where alias='anarchy'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'kingdom',
            'regimen', 'name_ti', 'kingdom',
            (select name_ti from regimen where alias='kingdom'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),(
            'The founder of the country is the king of life, no he will not get any coups. The king can only relinquished his title — selling country; buyer along with the country and takes the title of King (absolute monarch).',
            'regimen', 'descr_ti', 'kingdom',
            (select descr_ti from regimen where alias='kingdom'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'shogunate',
            'regimen', 'name_ti', 'shogunate',
            (select name_ti from regimen where alias='shogunate'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),(
            'Real power in the country belongs not to the king, and the samurai code of honor that Real Samurai.',
            'regimen', 'descr_ti', 'shogunate',
            (select descr_ti from regimen where alias='shogunate'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'principality',
            'regimen', 'name_ti', 'principality',
            (select name_ti from regimen where alias='principality'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),(
            'The owner of the country — prince or princess — enjoy great respect, but almost did not intervene in the affairs of people.',
            'regimen', 'descr_ti', 'principality',
            (select descr_ti from regimen where alias='principality'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'feudal system',
            'regimen', 'name_ti', 'feudal',
            (select name_ti from regimen where alias='feudal'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),(
            'Real power in the country belongs not to the king, and the Knights, the most romantic_flowers structure — here is thriving cult of beautiful ladies (ladies heart).',
            'regimen', 'descr_ti', 'feudal',
            (select descr_ti from regimen where alias='feudal'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'tribe',
            'regimen', 'name_ti', 'tribe',
            (select name_ti from regimen where alias='tribe'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),(
            'Tribe ruled by a leader.',
            'regimen', 'descr_ti', 'tribe',
            (select descr_ti from regimen where alias='tribe'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'деспостизм',
            'regimen', 'name_ti', 'despotism',
            (select name_ti from regimen where alias='despotism'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),(
            'Основатель страны является пожизненным ее хозяином; с одной стороны, это заманчиво создать такой тип страны, с другой стороны — в такой стране все остальные пользователи будут сильно ограничены в своих правах по сравнению с диктатором, поэтому вряд ли будет много желающих такую страну посетить.',
            'regimen', 'descr_ti','despotism',
            (select descr_ti from regimen where alias='despotism'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'монархия',
            'regimen', 'name_ti', 'monarchy',
            (select name_ti from regimen where alias='monarchy'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ), (
            'Oснователь страны становится ее первым монархом; со временем может произойти смена власти (переворот) — корона может перейти к другому, но только к тому, кто стал достаточно авторитетным в этой стране, т.е. имеет перед страной какие-то реальные заслуги.',
            'regimen', 'descr_ti', 'monarchy',
            (select descr_ti from regimen where alias='monarchy'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'султанат',
            'regimen', 'name_ti', 'sultanate',
            (select name_ti from regimen where alias='sultanate'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ), (
            'Восточная монархия. Oснователь страны является пожизненным ее хозяином; с одной стороны, это заманчиво создать такой тип страны, с другой стороны — в такой стране все остальные пользователи будут сильно ограничены в своих правах по сравнению с диктатором, поэтому вряд ли будет много желающих такую страну посетить.',
            'regimen', 'descr_ti', 'sultanate',
            (select descr_ti from regimen where alias='sultanate'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'республика',
            'regimen', 'name_ti', 'republic',
            (select name_ti from regimen where alias='republic'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),(
            'Избираемый президент. Президентом страны может стать каждый, не зависимо от авторитета (даже новичок) и реальных заслуг, главное — популярность, выбирается на определенный срок голосованием.',
            'regimen', 'descr_ti', 'republic',
            (select descr_ti from regimen where alias='republic'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'анархия',
            'regimen', 'name_ti', 'anarchy',
            (select name_ti from regimen where alias='anarchy'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),(
            'Основатель страны (владелец) ничем не отличается от других жителей, кроме права продать страну; купивший ее так же не будет иметь никаких привилегий, кроме права перепродать страну. Соответственно и жителям страны без разницы, кто ее владелец в настоящий момент.',
            'regimen', 'descr_ti', 'anarchy',
            (select descr_ti from regimen where alias='anarchy'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'королевство',
            'regimen', 'name_ti', 'kingdom',
            (select name_ti from regimen where alias='kingdom'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),(
            'Основатель страны является пожизненным королем, никакие перевороты ему не грозят. Король может только сам отказаться от своего титула — продав страну; покупатель вместе со страной приобретает и титул короля (абсолютного монарха).',
            'regimen', 'descr_ti', 'kingdom',
            (select descr_ti from regimen where alias='kingdom'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'сегунат',
            'regimen', 'name_ti', 'shogunate',
            (select name_ti from regimen where alias='shogunate'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),(
            'Фактическая власть в стране принадлежит не королю, а самураям, которые чтят Кодекс Настоящего Самурая.',
            'regimen', 'descr_ti', 'shogunate',
            (select descr_ti from regimen where alias='shogunate'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'графство',
            'regimen', 'name_ti', 'principality',
            (select name_ti from regimen where alias='principality'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),(
            'Владелец страны — граф или графиня — пользуются огромным уважением, но практически не вмешиваются в дела жителей.',
            'regimen', 'descr_ti', 'principality',
            (select descr_ti from regimen where alias='principality'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'феодальный строй',
            'regimen', 'name_ti', 'feudal',
            (select name_ti from regimen where alias='feudal'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),(
            'Фактическая власть в стране принадлежит не королю, а Рыцарскому Ордену; самый романтический строй — здесь процветает культ прекрасных дам (дам сердца).',
            'regimen', 'descr_ti', 'feudal',
            (select descr_ti from regimen where alias='feudal'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'племя',
            'regimen', 'name_ti', 'tribe',
            (select name_ti from regimen where alias='tribe'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),(
            'Племенем правит вождь.',
            'regimen', 'descr_ti', 'tribe',
            (select descr_ti from regimen where alias='tribe'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );

select 'log:topic' as log;

insert into topic(alias) values ('all');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'all topics',
            'topic',                     'name_ti','all',
            (select name_ti from topic where alias='all'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for all topics',
            'topic',                     'descr_ti','all',
            (select descr_ti from topic where alias='all'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'все темы',
            'topic',                      'name_ti','all',
            (select name_ti from topic  where alias='all'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'обсуждаем все темы',
            'topic',                      'descr_ti','all',
            (select descr_ti from topic  where alias='all'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        )
        ;

insert into topic(alias, parent_id)
    values
        ('sex',
            (select id from topic where alias='all')
        ),
        ('drugs',
            (select id from topic where alias='all')
        ),
        ('rock_n_roll',
            (select id from topic where alias='all')
        )
        ;

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'sex',
            'topic',                     'name_ti','sex',
            (select name_ti from topic where alias='sex'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for sex',
            'topic',                     'descr_ti','sex',
            (select descr_ti from topic where alias='sex'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'секс',
            'topic',                      'name_ti','sex',
            (select name_ti from topic  where alias='sex'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'обсуждаем секс',
            'topic',                      'descr_ti','sex',
            (select descr_ti from topic  where alias='sex'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'drugs',
            'topic',                     'name_ti','drugs',
            (select name_ti from topic where alias='drugs'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for drugs',
            'topic',                     'descr_ti','drugs',
            (select descr_ti from topic where alias='drugs'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'лекарства',
            'topic',                      'name_ti','drugs',
            (select name_ti from topic  where alias='drugs'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'все о лекарствах',
            'topic',                      'descr_ti','drugs',
            (select descr_ti from topic  where alias='drugs'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'rock_n_roll',
            'topic',                     'name_ti','rock_n_roll',
            (select name_ti from topic where alias='rock_n_roll'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for rock_n_roll',
            'topic',                     'descr_ti','rock_n_roll',
            (select descr_ti from topic where alias='rock_n_roll'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'музыка',
            'topic',                      'name_ti','rock_n_roll',
            (select name_ti from topic  where alias='rock_n_roll'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'все музыке',
            'topic',                      'descr_ti','rock_n_roll',
            (select descr_ti from topic  where alias='rock_n_roll'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        )
        ;

insert into topic(alias, parent_id)
    values
        ('classic_sex',
            (select id from topic where alias='sex')
        ),
        ('bdsm',
            (select id from topic where alias='sex')
        ),
        ('boobs',
            (select id from topic where alias='sex')
        )
        ;

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'classic sex',
            'topic',                     'name_ti','classic_sex',
            (select name_ti from topic where alias='classic_sex'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for classic sex',
            'topic',                     'descr_ti','classic_sex',
            (select descr_ti from topic where alias='classic_sex'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'секс',
            'topic',                      'name_ti','classic_sex',
            (select name_ti from topic  where alias='classic_sex'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'обсуждаем нормальный секс',
            'topic',                      'descr_ti','classic_sex',
            (select descr_ti from topic  where alias='classic_sex'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'bdsm',
            'topic',                     'name_ti','bdsm',
            (select name_ti from topic where alias='bdsm'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for bdsm',
            'topic',                     'descr_ti','bdsm',
            (select descr_ti from topic where alias='bdsm'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'бсдм',
            'topic',                      'name_ti','bdsm',
            (select name_ti from topic  where alias='bdsm'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'все о бсдм',
            'topic',                      'descr_ti','bdsm',
            (select descr_ti from topic  where alias='bdsm'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'boobs',
            'topic',                     'name_ti','boobs',
            (select name_ti from topic where alias='boobs'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for boobs',
            'topic',                     'descr_ti','boobs',
            (select descr_ti from topic where alias='boobs'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'cиськи',
            'topic',                      'name_ti','boobs',
            (select name_ti from topic  where alias='boobs'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'тема cисек',
            'topic',                      'descr_ti','boobs',
            (select descr_ti from topic  where alias='boobs'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        )
        ;

insert into topic(alias, parent_id)
    values
        ('meskalin',
            (select id from topic where alias='drugs')
        ),
        ('lsd',
            (select id from topic where alias='drugs')
        ),
        ('plan',
            (select id from topic where alias='drugs')
        )
        ;

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'meskalin',
            'topic',                     'name_ti','meskalin',
            (select name_ti from topic where alias='meskalin'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for meskalin',
            'topic',                     'descr_ti','meskalin',
            (select descr_ti from topic where alias='meskalin'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'домашняя химия',
            'topic',                      'name_ti','meskalin',
            (select name_ti from topic  where alias='meskalin'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'обсуждаем домашнюю химию',
            'topic',                      'descr_ti','meskalin',
            (select descr_ti from topic  where alias='meskalin'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'lsd',
            'topic',                     'name_ti','lsd',
            (select name_ti from topic where alias='lsd'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for lsd',
            'topic',                     'descr_ti','lsd',
            (select descr_ti from topic where alias='lsd'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'грибочки',
            'topic',                      'name_ti','lsd',
            (select name_ti from topic  where alias='lsd'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'все о грибах',
            'topic',                      'descr_ti','lsd',
            (select descr_ti from topic  where alias='lsd'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'plan',
            'topic',                     'name_ti','plan',
            (select name_ti from topic where alias='plan'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for plan',
            'topic',                     'descr_ti','plan',
            (select descr_ti from topic where alias='plan'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'лекарственные травы',
            'topic',                      'name_ti','plan',
            (select name_ti from topic  where alias='plan'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'тема о лекарственных травах',
            'topic',                      'descr_ti','plan',
            (select descr_ti from topic  where alias='plan'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        )
        ;

insert into topic(alias, parent_id)
    values
        ('new_wave',
            (select id from topic where alias='rock_n_roll')
        ),
        ('hard_rock',
            (select id from topic where alias='rock_n_roll')
        ),
        ('pop',
            (select id from topic where alias='rock_n_roll')
        )
        ;

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'new wave',
            'topic',                     'name_ti','new_wave',
            (select name_ti from topic where alias='new_wave'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for new wave',
            'topic',                     'descr_ti','new_wave',
            (select descr_ti from topic where alias='new_wave'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'новая волна',
            'topic',                      'name_ti','new_wave',
            (select name_ti from topic  where alias='new_wave'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'обсуждаем новую волну',
            'topic',                      'descr_ti','new_wave',
            (select descr_ti from topic  where alias='new_wave'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'hard rock',
            'topic',                     'name_ti','hard_rock',
            (select name_ti from topic where alias='hard_rock'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for hard rock',
            'topic',                     'descr_ti','hard_rock',
            (select descr_ti from topic where alias='hard_rock'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'тяжелый рок',
            'topic',                      'name_ti','hard_rock',
            (select name_ti from topic  where alias='hard_rock'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'все о тяжелом роке',
            'topic',                      'descr_ti','hard_rock',
            (select descr_ti from topic  where alias='hard_rock'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'pop',
            'topic',                     'name_ti','pop',
            (select name_ti from topic where alias='pop'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for pop',
            'topic',                     'descr_ti','pop',
            (select descr_ti from topic where alias='pop'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'попса',
            'topic',                      'name_ti','pop',
            (select name_ti from topic  where alias='pop'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'тема о попсе',
            'topic',                      'descr_ti','pop',
            (select descr_ti from topic  where alias='pop'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        )
        ;

select 'log:communitytype' as log;

insert into communitytype(alias)
    values
        ('common'), -- обычное
        ('secret'), -- секретное
        ('elite')   -- элитное
        ;

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'common',       'communitytype',     'name_ti','common',
            (select name_ti from communitytype where alias='common'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'secret',       'communitytype',     'name_ti','secret',
            (select name_ti from communitytype where alias='secret'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'обычное',      'communitytype',      'name_ti','common',
            (select name_ti from communitytype  where alias='common'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'тайное',       'communitytype',      'name_ti','secret',
            (select name_ti from communitytype  where alias='secret'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        );



select 'log:thingtype' as log;

insert into thingtype(alias) values ('all');

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'all thingtypes',
            'thingtype',                     'name_ti','all',
            (select name_ti from thingtype where alias='all'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for all thingtypes',
            'thingtype',                     'descr_ti','all',
            (select descr_ti from thingtype where alias='all'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'все вещи',
            'thingtype',                      'name_ti','all',
            (select name_ti from thingtype  where alias='all'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание всех вещей',
            'thingtype',                      'descr_ti','all',
            (select descr_ti from thingtype  where alias='all'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        )
        ;

insert into thingtype(alias, parent_id)
    values
        ('counter',
            (select id from thingtype where alias='all')
        ),
        ('closes',
            (select id from thingtype where alias='all')
        ),
        ('flowers',
            (select id from thingtype where alias='all')
        )
        ;

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'counter',
            'thingtype',                     'name_ti','counter',
            (select name_ti from thingtype where alias='counter'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for counter',
            'thingtype',                     'descr_ti','counter',
            (select descr_ti from thingtype where alias='counter'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'cчетчики',
            'thingtype',                      'name_ti','counter',
            (select name_ti from thingtype  where alias='counter'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание cчетчиков',
            'thingtype',                      'descr_ti','counter',
            (select descr_ti from thingtype  where alias='counter'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'closes',
            'thingtype',                     'name_ti','closes',
            (select name_ti from thingtype where alias='closes'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for closes',
            'thingtype',                     'descr_ti','closes',
            (select descr_ti from thingtype where alias='closes'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'одежда',
            'thingtype',                      'name_ti','closes',
            (select name_ti from thingtype  where alias='closes'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание одежды',
            'thingtype',                      'descr_ti','closes',
            (select descr_ti from thingtype  where alias='closes'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'flowers',
            'thingtype',                     'name_ti','flowers',
            (select name_ti from thingtype where alias='flowers'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for flowers',
            'thingtype',                     'descr_ti','flowers',
            (select descr_ti from thingtype where alias='flowers'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'цветы',
            'thingtype',                      'name_ti','flowers',
            (select name_ti from thingtype  where alias='flowers'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание цветов',
            'thingtype',                      'descr_ti','flowers',
            (select descr_ti from thingtype  where alias='flowers'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        )
        ;


insert into thingtype(alias, parent_id)
    values
        ('costume',
            (select id from thingtype where alias='closes')
        ),
        ('dress',
            (select id from thingtype where alias='closes')
        ),
        ('cap',
            (select id from thingtype where alias='closes')
        )
        ;

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'costume',
            'thingtype',                     'name_ti','costume',
            (select name_ti from thingtype where alias='costume'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for costume',
            'thingtype',                     'descr_ti','costume',
            (select descr_ti from thingtype where alias='costume'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'костюм',
            'thingtype',                      'name_ti','costume',
            (select name_ti from thingtype  where alias='costume'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание костюма',
            'thingtype',                      'descr_ti','costume',
            (select descr_ti from thingtype  where alias='costume'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'dress',
            'thingtype',                     'name_ti','dress',
            (select name_ti from thingtype where alias='dress'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for dress',
            'thingtype',                     'descr_ti','dress',
            (select descr_ti from thingtype where alias='dress'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'платье',
            'thingtype',                      'name_ti','dress',
            (select name_ti from thingtype  where alias='dress'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание платьев',
            'thingtype',                      'descr_ti','dress',
            (select descr_ti from thingtype  where alias='dress'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'cap',
            'thingtype',                     'name_ti','cap',
            (select name_ti from thingtype where alias='cap'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for cap',
            'thingtype',                     'descr_ti','cap',
            (select descr_ti from thingtype where alias='cap'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'шапка',
            'thingtype',                      'name_ti','cap',
            (select name_ti from thingtype  where alias='cap'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание шапок',
            'thingtype',                      'descr_ti','cap',
            (select descr_ti from thingtype  where alias='cap'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        )
        ;

insert into thingtype(alias, parent_id)
    values
        ('romantic_flowers',
            (select id from thingtype where alias='flowers')
        ),
        ('official_flowers',
            (select id from thingtype where alias='flowers')
        ),  
        ('funeral_flowers',
            (select id from thingtype where alias='flowers')
        )
        ;

insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'romantic flowers',
            'thingtype',                     'name_ti','romantic_flowers',
            (select name_ti from thingtype where alias='romantic_flowers'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for romantic flowers',
            'thingtype',                     'descr_ti','romantic_flowers',
            (select descr_ti from thingtype where alias='romantic_flowers'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'романтичные',
            'thingtype',                      'name_ti','romantic_flowers',
            (select name_ti from thingtype  where alias='romantic_flowers'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание романтичных цветов',
            'thingtype',                      'descr_ti','romantic_flowers',
            (select descr_ti from thingtype  where alias='romantic_flowers'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'official_flowers',
            'thingtype',                     'name_ti','official_flowers',
            (select name_ti from thingtype where alias='official_flowers'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for official_flowers',
            'thingtype',                     'descr_ti','official_flowers',
            (select descr_ti from thingtype where alias='official_flowers'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'официальные',
            'thingtype',                      'name_ti','official_flowers',
            (select name_ti from thingtype  where alias='official_flowers'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание официальных цветов',
            'thingtype',                      'descr_ti','official_flowers',
            (select descr_ti from thingtype  where alias='official_flowers'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'funeral_flowers',
            'thingtype',                     'name_ti','funeral_flowers',
            (select name_ti from thingtype where alias='funeral_flowers'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for funeral_flowers',
            'thingtype',                     'descr_ti','funeral_flowers',
            (select descr_ti from thingtype where alias='funeral_flowers'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'похоронные',
            'thingtype',                      'name_ti','funeral_flowers',
            (select name_ti from thingtype  where alias='funeral_flowers'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание похоронных цветов',
            'thingtype',                      'descr_ti','funeral_flowers',
            (select descr_ti from thingtype  where alias='funeral_flowers'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        )
        ;
        
        
        
select 'log:thing' as log;
        
insert into thing(alias, price, thingtype_id)
    values
        ('rose', 
            11,
            (select id from thingtype where alias='romantic_flowers')
        ),
        ('chamomile', 
            12,
            (select id from thingtype where alias='romantic_flowers')
        ),  
        ('simple_counter', 
            1000,
            (select id from thingtype where alias='counter')
        )
        ;
        
        
        
insert into tr (text, tt, tf, ta, ti, lang_id, trtype_id)
    values
        (   'rose',
            'thing',                     'name_ti','rose',
            (select name_ti from thing where alias='rose'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for rose',
            'thing',                     'descr_ti','rose',
            (select descr_ti from thing where alias='rose'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'роза',
            'thing',                      'name_ti','rose',
            (select name_ti from thing  where alias='rose'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание розы',
            'thing',                      'descr_ti','rose',
            (select descr_ti from thing  where alias='rose'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'chamomile',
            'thing',                     'name_ti','chamomile',
            (select name_ti from thing where alias='chamomile'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for chamomile',
            'thing',                     'descr_ti','chamomile',
            (select descr_ti from thing where alias='chamomile'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'ромашка',
            'thing',                      'name_ti','chamomile',
            (select name_ti from thing  where alias='chamomile'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание ромашки',
            'thing',                      'descr_ti','chamomile',
            (select descr_ti from thing  where alias='chamomile'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),
        (   'simple counter',
            'thing',                     'name_ti','simple_counter',
            (select name_ti from thing where alias='simple_counter'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),( 'for simple counter',
            'thing',                     'descr_ti','simple_counter',
            (select descr_ti from thing where alias='simple_counter'),
            (select id from lang where alias='en_gb'),
            (select id from trtype where alias='dynamic')
        ),
        (   'простой счетчик',
            'thing',                      'name_ti','simple_counter',
            (select name_ti from thing  where alias='simple_counter'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        ),( 'описание простого счетчика',
            'thing',                      'descr_ti','simple_counter',
            (select descr_ti from thing  where alias='simple_counter'),
            (select id from lang where alias='ru_ru'),
            (select id from trtype where alias='dynamic')
        )
        ;
        
        

select 'log:pers' as log;

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-100500, 'fadmin', 'sadmin', 'padmin@padmin.ru',
            093203230230, 'admin', 'admin', '21232F297A57A5A743894A0E4A801FC3');

select 'test users' as log;

-- insert into pers (id, fname, sname, email, phone, nick, login, phash)
--     values (1, 'fname-1', 'sname-1', 'email-1@email.ru',
--             293203230231, 'nick-1', 'login-1', '21232F297A57A5A743894A0E4A801FC3');
-- 

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-1001, 'test_1_fname-1', 'test_1_sname-1', 'test_1_email-1@email.ru',
            293203230231, 'test_1_nick-1', 'test_1_login-1', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-2001, 'test_1_fname-2', 'test_1_sname-2', 'test_1_email-2@email.ru',
            293203230231, 'test_1_nick-2', 'test_1_login-2', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-3001, 'test_1_fname-3', 'test_1_sname-3', 'test_1_email-3@email.ru',
            293203230231, 'test_1_nick-3', 'test_1_login-3', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-4001, 'test_1_fname-4', 'test_1_sname-4', 'test_1_email-4@email.ru',
            293203230231, 'test_1_nick-4', 'test_1_login-4', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-5001, 'test_1_fname-5', 'test_1_sname-5', 'test_1_email-5@email.ru',
            293203230231, 'test_1_nick-5', 'test_1_login-5', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-6001, 'test_1_fname-6', 'test_1_sname-6', 'test_1_email-6@email.ru',
            293203230231, 'test_1_nick-6', 'test_1_login-6', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-7001, 'test_1_fname-7', 'test_1_sname-7', 'test_1_email-7@email.ru',
            293203230231, 'test_1_nick-7', 'test_1_login-7', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-8001, 'test_1_fname-8', 'test_1_sname-8', 'test_1_email-8@email.ru',
            293203230231, 'test_1_nick-8', 'test_1_login-8', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-10001, 'test_1_fname-10', 'test_1_sname-10', 'test_1_email-10@email.ru',
            293203230231, 'test_1_nick-10', 'test_1_login-10', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-1002, 'test_2_fname-1', 'test_2_sname-1', 'test_2_email-1@email.ru',
            293203230231, 'test_2_nick-1', 'test_2_login-1', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-2002, 'test_2_fname-2', 'test_2_sname-2', 'test_2_email-2@email.ru',
            293203230231, 'test_2_nick-2', 'test_2_login-2', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-3002, 'test_2_fname-3', 'test_2_sname-3', 'test_2_email-3@email.ru',
            293203230231, 'test_2_nick-3', 'test_2_login-3', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-4002, 'test_2_fname-4', 'test_2_sname-4', 'test_2_email-4@email.ru',
            293203230231, 'test_2_nick-4', 'test_2_login-4', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-5002, 'test_2_fname-5', 'test_2_sname-5', 'test_2_email-5@email.ru',
            293203230231, 'test_2_nick-5', 'test_2_login-5', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-6002, 'test_2_fname-6', 'test_2_sname-6', 'test_2_email-6@email.ru',
            293203230231, 'test_2_nick-6', 'test_2_login-6', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-7002, 'test_2_fname-7', 'test_2_sname-7', 'test_2_email-7@email.ru',
            293203230231, 'test_2_nick-7', 'test_2_login-7', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-8002, 'test_2_fname-8', 'test_2_sname-8', 'test_2_email-8@email.ru',
            293203230231, 'test_2_nick-8', 'test_2_login-8', '21232F297A57A5A743894A0E4A801FC3');

insert into pers (id, fname, sname, email, phone, nick, login, phash)
    values (-10002, 'test_2_fname-10', 'test_2_sname-10', 'test_2_email-10@email.ru',
            293203230231, 'test_2_nick-10', 'test_2_login-10', '21232F297A57A5A743894A0E4A801FC3');




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


select 'log:pers2pgroup' as log;

insert into pers2pgroup (pers_id, group_id)
    values
        ((select id from pers where login='admin'),
            (select id from pgroup where alias='admin')),
        ((select id from pers where login='admin'),
            (select id from pgroup where alias='sysmsg')),
        ((select id from pers where login='admin'),
            (select id from pgroup where alias='contman'));


select 'log:paytype' as log;

insert into paytype(alias, isincome)
    values  ('roombet_out',     false),
            ('roombet_in',      true ),
            ('roomlot_in',      true ),
            ('thing_out',       false),
            ('exper_out',       false),
            ('room_out',        false),
            ('room_in',         true );

select 'log:treastype' as log;

insert into treastype (alias, isincome)
    values  ('transfer_in',     true),
            ('exper_out',       false),
            ('transfer_out',    false);

select 'log:transtype' as log;

insert into transtype (alias, isincome)
    values  ('to',     true),
            ('form',   false);

select 'log:filetype' as log;

insert into filetype (alias, mime, ext) values
    ('image/gif *.gif',   'image/gif', 'gif'),
    ('image/jpeg *.jpeg',  'image/jpeg', 'jpeg'),
    ('image/jpeg *.jpg',  'image/jpeg', 'jpg'),
    ('image/png *.png',   'image/png', 'png');


insert into fileinfotype(alias) values
    ('upload'),
    ('download'),
    ('filesystem');


insert into geo(alias) values
    ('ru'),
    ('kz'),
    ('uk'),
    ('by'),
    ('ua'),
    ('jp'),
    ('cn');

insert into geo(alias, parent_id) values
    ('moscow',      (select id from geo where alias = 'ru')),
    ('rudnyj',      (select id from geo where alias = 'ru')),
    ('kazan',       (select id from geo where alias = 'ru')),
    ('anadyr',      (select id from geo where alias = 'ru')),
    ('astana',      (select id from geo where alias = 'kz')),
    ('alma-ata',    (select id from geo where alias = 'kz')),
    ('rudnyj',      (select id from geo where alias = 'kz')),
    ('misk',        (select id from geo where alias = 'by')),
    ('brest',       (select id from geo where alias = 'by')),
    ('kiev',        (select id from geo where alias = 'ua')),
    ('odessa',      (select id from geo where alias = 'ua'));

/**
    При создании пользователя помещаем его в комнату для новичков
    @depricated перенесено в триггеры
**/
-- alter table pers alter column live_room_id set default noobsroom();

create or replace function utcnow() returns timestamp as $$
        select now() at time zone 'UTC'
$$ language sql;


/*
create or replace function doc() returns setof doc as $$
        select * from doc;
$$ language sql;

create or replace function vcup(
    i numeric, 
    t varchar, 
    f varchar default 'vcounter') 
returns numeric as $$
begin
    execute 'update '||quote_ident(t)
    ||' set '||quote_ident(f)||' = '||quote_ident(f)||' + 1'
    ||' where id =  '||cast(i as varchar)||';';
    return i;
end; $$ language plpgsql;

create or replace view pers_v as
    select pers.*, from doc 
        left join doctype on
            doctype.id = doc.doctype_id
        left join contype on
            contype.id = doc.contype_id;

create or replace view doc_v as
    select doc.*, doctype.alias as doctype_alias, contype.alias as contype_alias from doc 
        left join doctype on
            doctype.id = doc.doctype_id
        left join contype on
            contype.id = doc.contype_id;

create or replace view room_v as
    select doc.*, doctype.alias as doctype_alias, contype.alias as contype_alias from doc 
        left join doctype on
            doctype.id = doc.doctype_id
        left join contype on
            contype.id = doc.contype_id;

create or replace view pers_v as
    select pers.*, from doc 
        left join doctype on
            doctype.id = doc.doctype_id
        left join contype on
            contype.id = doc.contype_id;
            pstatus_id*/