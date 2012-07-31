/***********************************************************************
*
* \file Вставка данных в базу
*
***********************************************************************/

insert into perm_type (name)
    values
        ('static');

insert into perm_entity_type (name)
    values
        ('user');

insert into perm (name, description, perm_type_id, entity_type_id)
    values
        ('system', 'права разрещенные только для самой системы',
            (select id from perm_type where name='static'),
            (select id from perm_entity_type where name='user')
        ),
        ('admin', 'полный доступ',
            (select id from perm_type where name='static'),
            (select id from perm_entity_type where name='user')
        ),
        ('undel_group', 'не удаляемые группы',
            (select id from perm_type where name='static'),
            (select id from perm_entity_type where name='user')
        ),
        ('undel_user', 'не удаляемые пользователи',
            (select id from perm_type where name='static'),
            (select id from perm_entity_type where name='user')
        ),
        ('insider', 'доступ к личному кабинету',
            (select id from perm_type where name='static'),
            (select id from perm_entity_type where name='user')
        ),
        ('sysmsg', 'право получать системные сообщения',
            (select id from perm_type where name='static'),
            (select id from perm_entity_type where name='user')
        ),
        ('sysconfiger', 'право изменять системные настройки',
            (select id from perm_type where name='static'),
            (select id from perm_entity_type where name='user')
        );


insert into user_group (name, description)
    values
        ('admin', 	         'администраторы'),
        ('sysmsg',           'получатели системных сообщений'),
        ('insider',          'пользователи');

insert into perm2group (perm_id, group_id)
    values
        ( (select id from perm where name='admin'),
            (select id from user_group where name='admin')),
        ( (select id from perm where name='undel_group'),
            (select id from user_group where name='admin')),
        ( (select id from perm where name='undel_user'),
            (select id from user_group where name='admin')),
        ( (select id from perm where name='sysmsg'),
            (select id from user_group where name='admin')),
        ( (select id from perm where name='insider'),
            (select id from user_group where name='admin')),
        ( (select id from perm where name='undel_group'),
            (select id from user_group where name='sysmsg')),
        ( (select id from perm where name='sysmsg'),
            (select id from user_group where name='sysmsg')),
        ( (select id from perm where name='insider'),
            (select id from user_group where name='sysmsg')),
        ( (select id from perm where name='undel_group'),
            (select id from user_group where name='insider')),
        ( (select id from perm where name='insider'),
            (select id from user_group where name='insider'));


insert into user_ (fname, sname, email, nick, phash)
    values ('fadmin', 'ladmin', 'padmin',
        'admin', '21232F297A57A5A743894A0E4A801FC3');

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


insert into user2group (user_id, group_id)
    values
        ((select id from user_ where login='admin'),
            (select id from user_group where name='admin')),
        ((select id from user_ where login='admin'),
            (select id from user_group where name='sysmsg')),
        ((select id from user_ where login='admin'),
            (select id from user_group where name='insider'));

