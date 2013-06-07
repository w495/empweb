%% Author: w-495
%% Created: 25.07.2012
%% Description: TODO: Add description to biz_user
-module(empdb_biz_file).

%% ===========================================================================
%% Заголовочные файлы
%% ===========================================================================

%%
%% Структры для работы с запросами к базе данных
%%
-include("empdb.hrl").


%% ==========================================================================
%% Экспортируемые функции
%% ==========================================================================

%%
%% Блоги
%%
-export([
    md5/1,
    get/1,
    get/2,
    get_system_picture/2,
    create_copy_worker/1,
    get_handle_pictures/6,
    get_handle_picture/6,
    create/1,
    delete/1,
    update/1
]).



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                          ЗНАЧИМЫЕ ОБЪЕКТЫ
%%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(Params)->
    Fsdir = ?EMPDB_BIZ_FILE_FSDIR,
    Dldir = ?EMPDB_BIZ_FILE_DLDIR,

    io:format("~n~n~nParams = ~p ~n~n~n", [proplists:delete(filebody, Params)]),


    Doc_id   = proplists:get_value(doc_id,   Params, null),
    Owner_id = proplists:get_value(owner_id, Params, null),

    %% Получаем данные о загружаемом файле
    Ulname  = proplists:get_value(filename, Params, []),
    Ulbody  = proplists:get_value(filebody, Params, []),
    Ulext   = proplists:get_value(fileextension, Params, []),
    Ulsize  = erlang:byte_size(Ulbody),



    Md5binary = md5(Ulbody),
    Path_binary  = rand_bytes(),

    Path_list =
        [[ io_lib:format("~.36.0b", [X])] || <<X>> <=  Path_binary],

    %% Конструируем пути для файла в файловой системе.
    Fspath         = erlang:list_to_binary(filename:join(Path_list)),
    Fspath_ext     =  << Fspath/binary, $., Ulext/binary >>,
    Fspath_full    =  << Fsdir/binary, Fspath_ext/binary >>,

    %spawn_link(fun()->
        ok = filelib:ensure_dir(Fspath_full),
        ok = file:write_file(Fspath_full, Ulbody),
    %end),

    %% Конструируем пути для файла для скачивания.
    Dlpath         = erlang:list_to_binary(string:join(Path_list,[<<"/">>])),
    Dlpath_ext     = << Dlpath/binary, $., Ulext/binary  >>,
    Dlpath_full    = << Dldir/binary, Dlpath_ext/binary >>,

    Token_string =
        erlang:list_to_binary(
            [ io_lib:format("~2.16.0b", [X]) || <<X>> <=  Path_binary]
        ),
    Token_long =
        erlang:list_to_integer(erlang:binary_to_list(Token_string), 16),
    Md5string =
        erlang:list_to_binary(
            [ io_lib:format("~2.16.0b", [X]) || <<X>> <=  Md5binary]
        ),
    Md5long =
        erlang:list_to_integer(erlang:binary_to_list(Md5string), 16),

    empdb_dao:with_transaction(fun(Con)->
        %% -------------------------------------------------------------------
        %% Выбираем тип файла.
        %% -------------------------------------------------------------------
        {ok, [{Filetypepl}]} = empdb_dao_filetype:get(Con, [
            {ext, Ulext},
            {limit, 1}
        ]),

        Filetype_id =
            proplists:get_value(id,           Filetypepl),
        Filetype_mimesuptype =
            proplists:get_value(mimesuptype,  Filetypepl),


        %% -------------------------------------------------------------------
        %% Создаем контейнер, куда помещаем описания файлов
        %% -------------------------------------------------------------------
        {ok, [{File}]} = empdb_dao_file:create(Con, [
            {doc_id,            Doc_id},
            {owner_id,          Owner_id},
            {tokenstring,       Token_string},
            {tokenlong,         Token_long}
        ]),

        File_id = proplists:get_value(id, File),



        io:format("~n~nFspath = ~p~n~n", [Fspath_full]),
        Whpl =
            case Filetype_mimesuptype of
                <<"image">> ->
                    Whpl_ = gm:identify(Fspath_full, [width, height]),
                    {ok, [{_}]} = empdb_dao_fileinfo:create(Con, [
                        {fileinfotype_alias,    filesystem},
                        {doc_id,                Doc_id},
                        {owner_id,              Owner_id},
                        {filetype_id,           Filetype_id},
                        {'size',                Ulsize},
                        {tokenstring,           Token_string},
                        {tokenlong,             Token_long},
                        {md5string,             Md5string},
                        {md5long,               Md5long},
                        {dir,                   Fsdir},
                        {file_id,               File_id},
                        {path,                  Fspath_ext},
                        {image_width,           null},
                        {image_height,          null}
                    ]),
                    {ok, [{_}]} = empdb_dao_fileinfo:create(Con, [
                        {fileinfotype_alias,    download},
                        {doc_id,                Doc_id},
                        {owner_id,              Owner_id},
                        {filetype_id,           Filetype_id},
                        {'size',                Ulsize},
                        {tokenstring,           Token_string},
                        {tokenlong,             Token_long},
                        {md5string,             Md5string},
                        {md5long,               Md5long},
                        {dir,                   Dldir},
                        {file_id,               File_id},
                        {path,                  Dlpath_ext},
                        {image_width,           null},
                        {image_height,          null}
                    ]),
                    [
                        {image_width,  proplists:get_value(width, Whpl_) },
                        {image_height, proplists:get_value(height, Whpl_)}
                    ];
                _ ->
                    [
                        {image_width, null},
                        {image_height, null}
                    ]
            end,


        %% -------------------------------------------------------------------
        %% Создаем описание файла,
        %% который собираемся хранить в файловой системе
        %% -------------------------------------------------------------------
        {ok, [{Fsfileinfopl}]} = empdb_dao_fileinfo:create(Con, [
            {fileinfotype_alias,    filesystem},
            {doc_id,                Doc_id},
            {owner_id,              Owner_id},
            {filetype_id,           Filetype_id},
            {'size',                Ulsize},
            {tokenstring,           Token_string},
            {tokenlong,             Token_long},
            {md5string,             Md5string},
            {md5long,               Md5long},
            {dir,                   Fsdir},
            {file_id,               File_id},
            {path,                  Fspath_ext}
            |Whpl
        ]),


        %% -------------------------------------------------------------------
        %% Создаем описание файла,
        %% который собираемся отдавать пользователю
        %% -------------------------------------------------------------------
        {ok, [{Dlfileinfopl}]} = empdb_dao_fileinfo:create(Con, [
            {fileinfotype_alias,    download},
            {doc_id,                Doc_id},
            {owner_id,              Owner_id},
            {filetype_id,           Filetype_id},
            {'size',                Ulsize},
            {tokenstring,           Token_string},
            {tokenlong,             Token_long},
            {md5string,             Md5string},
            {md5long,               Md5long},
            {dir,                   Dldir},
            {file_id,               File_id},
            {path,                  Dlpath_ext}
            |Whpl
        ]),


        %% -------------------------------------------------------------------
        %% Создаем описание файла, который загрузил пользователь.
        %% -------------------------------------------------------------------
        {ok, [{Ulfileinfopl}]} = empdb_dao_fileinfo:create(Con, [
            {fileinfotype_alias,    upload},
            {doc_id,                Doc_id},
            {owner_id,              Owner_id},
            {filetype_id,           Filetype_id},
            {'size',                Ulsize},
            {tokenstring,           Token_string},
            {tokenlong,             Token_long},
            {md5string,             Md5string},
            {md5long,               Md5long},
            {file_id,               File_id},
            {name,                  Ulname}
            |Whpl
        ]),
%
%
%         spawn_link(fun()->
%             lists:map(
%                 fun({W, H})->
%                     spawn_link(fun()->
%                         create_copy([
%                             {fs_path_full,  Fspath_full},
%                             {file_id,       File_id},
%                             {doc_id,        Doc_id},
%                             {owner_id,      Owner_id},
%                             {fileextension, Ulext},
%                             {image_width,   W},
%                             {image_height,  H},
%                             {ul_name,       Ulname}
%                         ])
%                     end),
%                     spawn_link(fun()->
%                         create_copy([
%                             {fs_path_full,  Fspath_full},
%                             {file_id,       File_id},
%                             {doc_id,        Doc_id},
%                             {owner_id,      Owner_id},
%                             {fileextension, Ulext},
%                             {image_width,   erlang:trunc(W*0.98)},
%                             {image_height,  erlang:trunc(H*0.98)},
%                             {ul_name,       Ulname}
%                         ])
%                     end)
%                 end,
%                 [
%                     {320, 240},
%                     {480, 320},
%                     {220, 176},
%                     {128, 128}
%                 ]
%             )
%         end),

        case proplists:get_value(isres,   Params, null) of
            true ->
                {ok, [{[
                    {file_id,       File_id},
                    {path,          erlang:list_to_binary([Dldir, Dlpath_ext])},
                    {originalname,  Ulname},
                    {md5sum,        Md5string}
                ]}]};
            _ ->
                {ok, [{[
                    {file_id,       File_id},
                    {originalname,  Ulname},
                    {md5sum,        Md5string}
                ]}]}
        end
    end).


gm_convert_geometry(null, null) ->
    {error, null};

gm_convert_geometry(null, H) ->
    {ok,
        erlang:list_to_binary([
            <<"x">>,
            empdb_convert:to_binary(H)
        ])
    };

gm_convert_geometry(W, null) ->
    {ok,
        erlang:list_to_binary([
            empdb_convert:to_binary(W),
            <<"x ">>
        ])
    };

gm_convert_geometry(W, H) ->
    {ok,
        erlang:list_to_binary([
            empdb_convert:to_binary(W),
            <<"x">>,
            empdb_convert:to_binary(H)
        ])
    }.

gm_convert_geometry(
    Orig_fs_path_full,
    Fspath_full,
    Image_width,
    Image_height
) ->
    case gm_convert_geometry(Image_width, Image_height) of
        {error, Reason} ->
            io:format("~n ~n Reason = ~p ~n ~n", [ Reason ]),
            Reason;
        {ok, Geometry} ->
            gm:convert(
                Orig_fs_path_full,
                Fspath_full,
                [{geometry,
                    erlang:list_to_binary([
                        Geometry,
                        <<" >">>
                    ])
                }]
            )
    end.

gm_convert_tge(
    Orig_fs_path_full,
    Fspath_full,
    Image_width,
    Image_height
) ->
    gm:convert(
        Orig_fs_path_full,
        Fspath_full,
        [
            {thumbnail, Image_width, Image_height},
            {gravity, <<"center">>},
            {extent, Image_width, Image_height},
            {background, <<"transparent">>}
        ]
    ).

gm_convert(
    Orig_fs_path_full,
    Fspath_full,
    null,
    Image_height
)->
    gm_convert_geometry(Orig_fs_path_full,Fspath_full,null,Image_height);

gm_convert(
    Orig_fs_path_full,
    Fspath_full,
    Image_width,
    null
)->
    gm_convert_geometry(Orig_fs_path_full,Fspath_full,Image_width,null);


gm_convert(
    Orig_fs_path_full,
    Fspath_full,
    Image_width,
    Image_height
)->
    gm_convert_tge(Orig_fs_path_full,Fspath_full,Image_width,Image_height).


convert(Params)->
    Orig_fs_path_full   = proplists:get_value(orig_fs_path_full, Params),
    Fspath_full        = proplists:get_value(fs_path_full, Params),
    Image_width         = proplists:get_value(image_width, Params),
    Image_height         = proplists:get_value(image_height, Params),

    io:format("~n ~n convert(Params) = ~p ~n ~n", [ Params ]),

    gm_convert(
        Orig_fs_path_full,
        Fspath_full,
        Image_width,
        Image_height
    ).


create_copy(Params)->
    Fspath_full     = proplists:get_value(fs_path_full, Params),
    File_id         = proplists:get_value(file_id,          Params, null),
    Doc_id          = proplists:get_value(doc_id,           Params, null),
    Owner_id        = proplists:get_value(owner_id,         Params, null),
    Ext             = proplists:get_value(fileextension,    Params, []),
    Image_width     = proplists:get_value(image_width,      Params, null),
    Image_height    = proplists:get_value(image_height,     Params, null),
    Connection      = proplists:get_value(connection,       Params, null),

    create_copy_worker([
        {connection,    Connection},
        {fs_path_full,  Fspath_full},
        {file_id,       File_id},
        {doc_id,        Doc_id},
        {owner_id,      Owner_id},
        {fileextension, Ext},
        {image_width,   Image_width},
        {image_height,  Image_height}
    ]),
    create_copy_worker([
        {connection,    Connection},
        {fs_path_full,  Fspath_full},
        {file_id,       File_id},
        {doc_id,        Doc_id},
        {owner_id,      Owner_id},
        {fileextension, Ext},
        {image_width,   Image_width},
        {image_height,  null}
    ]),
    create_copy_worker([
        {connection,    Connection},
        {fs_path_full,  Fspath_full},
        {file_id,       File_id},
        {doc_id,        Doc_id},
        {owner_id,      Owner_id},
        {fileextension, Ext},
        {image_width,   null},
        {image_height,  Image_height}
    ]),
    ok.


find_ext(Filename)->
    case re:run(Filename, ".+[.](.+)$", []) of
        {match,[_,Poslen1]} ->
            binary:part(Filename, Poslen1);
        _ ->
            <<"undefined">>
    end.

filter_value(null, Filename)->
    find_ext(Filename);

filter_value(<<>>, Filename)->
    find_ext(Filename);

filter_value(Result, _)->
    Result.

create_copy_worker(Params)->
    Fsdir = ?EMPDB_BIZ_FILE_FSDIR,
    Dldir = ?EMPDB_BIZ_FILE_DLDIR,

    io:format("~n ~n Params = ~p ~n~n", [ Params ]),


    File_id         = proplists:get_value(file_id,          Params, null),
    Doc_id          = proplists:get_value(doc_id,           Params, null),
    Owner_id        = proplists:get_value(owner_id,         Params, null),
    Image_width     = proplists:get_value(image_width,      Params, null),
    Image_height    = proplists:get_value(image_height,     Params, null),
    Connection      = proplists:get_value(connection,       Params, null),


    Orig_fs_path = proplists:get_value(fs_path, Params, <<>>),
    Orig_fs_dir  = proplists:get_value(fs_dir,  Params, <<>>),

    Orig_fs_path_full = proplists:get_value(fs_path_full, Params,
        << Orig_fs_dir/binary, Orig_fs_path/binary >>),

    Ext = filter_value(proplists:get_value(fileextension,    Params, <<>>), Orig_fs_path_full),


    Path_binary  = rand_bytes(),



    io:format("~n ~n Image_width = ~p ~p ~n~n", [ Image_width, Image_height ]),


    Path_list =
        [[ io_lib:format("~.36.0b", [X])] || <<X>> <=  Path_binary],



    io:format("~n ~n Ext = ~w ~n~n", [ Ext ]),


    io:format("~n ~n Path_list = ~w ~n~n", [ Path_list ]),


    %% Конструируем пути для файла в файловой системе.
    Fspath         = erlang:list_to_binary(filename:join(Path_list)),

    io:format("~n ~n Fspath = ~w ~n ~n", [ Fspath ]),

    Fspath_ext     =  << Fspath/binary, $., Ext/binary >>,

    io:format("~n ~n Fspath_ext = ~w ~n ~n", [ Fspath_ext ]),


    Fspath_full    =  << Fsdir/binary, Fspath_ext/binary >>,

    io:format("~n ~n Fspath_full = ~p ~n ~n", [ Fspath_full ]),


    io:format("~n ~n Orig_fs_path_full = ~p ~n ~n", [ Orig_fs_path_full ]),


    ok = filelib:ensure_dir(Fspath_full),
    convert([
        {orig_fs_path_full, Orig_fs_path_full},
        {fs_path_full,      Fspath_full},
        {image_width,       Image_width},
        {image_height,      Image_height}
    ]),

    {ok, Fsbody} = file:read_file(Fspath_full),
    Md5binary = md5(Fsbody),

    Fssize  = erlang:byte_size(Fsbody),

    %% Конструируем пути для файла для скачивания.
    Dlpath         = erlang:list_to_binary(string:join(Path_list,[<<"/">>])),
    Dlpath_ext     = << Dlpath/binary, $., Ext/binary  >>,
    Dlpath_full    = << Dldir/binary, Dlpath_ext/binary >>,

    Token_string =
        erlang:list_to_binary(
            [ io_lib:format("~2.16.0b", [X]) || <<X>> <=  Path_binary]
        ),
    Token_long =
        erlang:list_to_integer(erlang:binary_to_list(Token_string), 16),
    Md5string =
        erlang:list_to_binary(
            [ io_lib:format("~2.16.0b", [X]) || <<X>> <=  Md5binary]
        ),
    Md5long =
        erlang:list_to_integer(erlang:binary_to_list(Md5string), 16),

    Iwidth  = Image_width,
    Iheight = Image_height,

    Daoaction=
        fun(Con)->
            {ok, [{Filetypepl}]} = empdb_dao_filetype:get(Con, [
                {ext, Ext},
                {limit, 1}
            ]),
            Filetype_id =
                proplists:get_value(id,           Filetypepl),
            Filetype_mimesuptype =
                proplists:get_value(mimesuptype,  Filetypepl),
            %% -------------------------------------------------------------------
            %% Создаем описание файла,
            %% который собираемся хранить в файловой системе
            %% -------------------------------------------------------------------
            {ok, [{Fsfileinfopl}]} = empdb_dao_fileinfo:create(Con, [
                {fileinfotype_alias,    filesystem},
                {doc_id,                Doc_id},
                {owner_id,              Owner_id},
                {filetype_id,           Filetype_id},
                {'size',                Fssize},
                {tokenstring,           Token_string},
                {tokenlong,             Token_long},
                {md5string,             Md5string},
                {md5long,               Md5long},
                {dir,                   Fsdir},
                {file_id,               File_id},
                {path,                  Fspath_ext},
                {image_width,           Iwidth},
                {image_height,          Iheight}
            ]),
            %% -------------------------------------------------------------------
            %% Создаем описание файла,
            %% который собираемся отдавать пользователю
            %% -------------------------------------------------------------------
            {ok, [{Dlfileinfopl}]} = empdb_dao_fileinfo:create(Con, [
                {fileinfotype_alias,    download},
                {doc_id,                Doc_id},
                {owner_id,              Owner_id},
                {filetype_id,           Filetype_id},
                {'size',                Fssize},
                {tokenstring,           Token_string},
                {tokenlong,             Token_long},
                {md5string,             Md5string},
                {md5long,               Md5long},
                {dir,                   Dldir},
                {file_id,               File_id},
                {path,                  Dlpath_ext},
                {image_width,           Iwidth},
                {image_height,          Iheight}
            ]),
            {ok, [{[
                {file_id,       File_id},
                %{path,          erlang:list_to_binary([Dldir, Dlpath_ext])},
                {dir,           Dldir},
                {path,          Dlpath_ext},
                {image_width,   Image_width},
                {image_height,  Image_height},
                {dl_dir,        Dldir},
                {dl_path_ext,   Dlpath_ext},
                {md5sum,        Md5string}
            ]}]}
        end,

    case Connection of
        null ->
            empdb_dao:with_transaction(Daoaction);
        Con ->
            Daoaction(Con)
    end.



get_handle_pictures(Con, Phobjs, What, Fields, _, _) ->
    lists:map(
        fun(Phobj)->
            get_handle_picture(Con, Phobj, What, Fields, [], [])
        end,
        Phobjs
    ).

get_handle_picture(Con, {Phobjpl}, What, Fields, _, _) ->

    File_id = proplists:get_value(file_id, Phobjpl),
    Req_width     = proplists:get_value(image_width, What, null),
    Req_height    = proplists:get_value(image_height, What, null),

    Options    = proplists:get_value(options, What, []),



    case empdb_dao_fileinfo:get(Con, [
        {file_id, File_id},
        {fileinfotype_alias, download},
        {image_width, Req_width},
        {image_height, Req_height},
        {limit, 1}
    ]) of
        {ok, []} ->
            io:format("~n~n~n !!!!!!!   !!!!!!!! ~n~n~n"),
            Fs_dir   = proplists:get_value(dir,          Phobjpl, <<>>),
            Fs_path  = proplists:get_value(path,         Phobjpl, <<>>),
            File_id  = proplists:get_value(file_id,      Phobjpl, null),
            Doc_id   = proplists:get_value(doc_id,       Phobjpl, null),
            Owner_id = proplists:get_value(owner_id,     Phobjpl, null),
            Ext      = proplists:get_value(filetype_ext, Phobjpl, <<>>),
            {ok, [{Copypl}]} =
                empdb_biz_file:create_copy_worker([
                    {connection,    Con},
                    {fs_path,       Fs_path},
                    {fs_dir,        Fs_dir},
                    {file_id,       File_id},
                    {doc_id,        Doc_id},
                    {owner_id,      Owner_id},
                    {fileextension, Ext},
                    {image_width,   Req_width},
                    {image_height,  Req_height}
                ]),
            get_transform(Copypl, Phobjpl, Fields, Options);
        {ok, [{Respl}]} ->
            io:format("Respl = ~p~n~n", [Respl]),

            get_transform(Respl, Phobjpl, Fields, Options);
        Error ->
            Error
    end.

get_transform(Res, Phobjpl, Fields, Options) ->
    Newphobjpl =
        lists:keyreplace(path, 1,
            lists:keyreplace(dir, 1,
                lists:keyreplace(image_height, 1,
                    lists:keyreplace(image_width, 1,
                        Phobjpl,
                        {image_width,
                            proplists:get_value(image_width, Res)
                        }
                    ),
                    {image_height,
                        proplists:get_value(image_height, Res)
                    }
                ),
                {dir,
                    proplists:get_value(dir, Res)
                }
            ),
            {path,
                proplists:get_value(path, Res)
            }
        ),
    get_handle_pl(Newphobjpl, Fields, Options).


get_handle_pl(Phpl, Fields, Options) ->
    io:format("Phpl = ~p ~n~n~n", [Phpl]),

    get_handle_path((lists:member(path, Fields) or (Fields =:= [])), Phpl, Options).

get_handle_pathtuple(Phpl, Options) ->
   %0 io:format("Phpl = ~p ~n~n~n", [Phpl]),
    Outpathname   = proplists:get_value(outpathname, Options, path),
    {Outpathname,
        <<  (proplists:get_value(dir, Phpl))/binary,
            (proplists:get_value(path, Phpl))/binary
        >>
    }.

%get_handle_pathtuple(Name, Phpl) ->
    %{Name,
        %<<  (proplists:get_value(dir, Phpl))/binary,
            %(proplists:get_value(path, Phpl))/binary
        %>>
    %}.

get_handle_path_(Phpl, Options) ->
    [
        get_handle_pathtuple(Phpl, Options) | Phpl
    ].

get_handle_path(true, Phpl, Options) ->
    {[
        get_handle_pathtuple(Phpl, Options)
        | proplists:delete(dir,
                    proplists:delete(path,
                        proplists:delete(path,
                            proplists:delete(fs_path_full,
                                proplists:delete(fileextension, Phpl)))))
    ]};

get_handle_path(_, Phpl, _) ->
    {proplists:delete(dir,
        proplists:delete(path,
            proplists:delete(fs_path_full,
                proplists:delete(fileextension, Phpl))))}.


get_system_picture(Con, What) ->

    Fields =
        proplists:get_value(
            fields,
            What,
            [image_width, image_height, file_id, path]
        ),

    Req_width     = proplists:get_value(image_width, What, null),
    Req_height    = proplists:get_value(image_height, What, null),

    case empdb_dao:get([
        {empdb_dao_file, id},
        {empdb_dao_fileinfo, file_id}
    ],Con,[
        {fields, [
            fileinfotype_alias,'fileinfo.filetype_ext',
            {as, {'fileinfo.path', path}},
            {as, {'fileinfo.dir',  dir}}
            | proplists:delete(path, Fields)
        ]},
        {fileinfotype_alias, filesystem},
        {image_height, null},
        {image_width, null}

            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %         {'or', [
            %             {image_width, Req_width},
            %             {image_width, null}
            %         ]},
            %         {'or', [
            %             {image_height, Req_height},
            %             {image_height, null}
            %         ]}
            % {image_width, proplists:get_value(image_width,      What, null)},
            % {image_height, proplists:get_value(image_height,     What, null)},
            % {fileinfotype_alias, download}
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        |proplists:delete(fields,
            proplists:delete(image_height,
                proplists:delete(image_width, What)))

    ]) of
        {ok,Phobjs} ->
            {ok, empdb_biz_file:get_handle_pictures(Con, Phobjs, What, Fields, Req_width, Req_height)};
        Error ->
            Error
    end.


update(Params)->
    ok.
get(Params)->
    ok.
get(Params, Fileds)->
    ok.
get_album_adds(Con, Getresult) ->
    ok.
delete(Params)->
    ok.
is_owner(Uid, Oid)->
    ok.


rand_bytes()->
    crypto:rand_bytes(crypto:rand_uniform(16, 32)).

rand_bytes(Md5binary)->
    <<  (crypto:rand_bytes(crypto:rand_uniform(1, 8)))/binary,
        Md5binary/binary,
        (crypto:rand_bytes(crypto:rand_uniform(1, 8)))/binary
    >>.

md5(Bin) ->
    C1 = erlang:md5_init(),
    C2 = erlang:md5_update(C1, Bin),
    C3 = erlang:md5_final(C2),
    C3.
