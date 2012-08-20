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
        ('en', 'english language'),
        ('ru', 'russian language');


insert into trtype (alias, descr)
    values
        ('static',  'static translation'),
        ('dynamic', 'dynamic translation');




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

insert into permentitytype (alias)
    values
        ('pers');

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

insert into pgroup (alias)
    values
        ('admin'),
        ('sysmsg'),
        ('sysconfiger'),
        ('contman');

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

-------------------------------------------------------------------------------
-- Документы
-------------------------------------------------------------------------------

insert into acctype (alias)
    values ('private'), ('protected'), ('public');

insert into contype(alias)
    values ('common'), ('adult_only');

insert into doctype(alias)
    values ('blog'), ('blog_comment'), ('gallery'), ('photo'), ('photo_comment'), ('attach_descr');

