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
    get/1,
    get/2,
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
    Fs_dir = <<"deps/empdb/priv/data/">>,
    Dl_dir = <<"/jsonapi/photo/">>,

    Doc_id   = proplists:get_value(doc_id,   Params, null),
    Owner_id = proplists:get_value(owner_id, Params, null),

    %% Получаем данные о загружаемом файле
    Ul_name  = proplists:get_value(filename, Params, []),
    Ul_body  = proplists:get_value(filebody, Params, []),
    Ul_ext   = proplists:get_value(fileextension, Params, []),
    Ul_size  = erlang:byte_size(Ul_body),

    Md5_binary = erlang:md5(Ul_body),
    Path_binary  =
        <<  (crypto:rand_bytes(crypto:rand_uniform(1, 8)))/binary,
            Md5_binary/binary,
            (crypto:rand_bytes(crypto:rand_uniform(1, 8)))/binary
        >>,
    Path_list =
        [[ io_lib:format("~.36.0b", [X])] || <<X>> <=  Path_binary],

    %% Конструируем пути для файла в файловой системе.
    Fs_path         = erlang:list_to_binary(filename:join(Path_list)),
    Fs_path_ext     =  << Fs_path/binary, $., Ul_ext/binary >>,
    Fs_path_full    =  << Fs_dir/binary, Fs_path_ext/binary >>,

    spawn_link(fun()->
        ok = filelib:ensure_dir(Fs_path_full),
        ok = file:write_file(Fs_path_full, Ul_body)
    end),

    %% Конструируем пути для файла для скачивания.
    Dl_path         = erlang:list_to_binary(string:join(Path_list,[<<"/">>])),
    Dl_path_ext     = << Dl_path/binary, $., Ul_ext/binary  >>,
    Dl_path_full    = << Dl_dir/binary, Dl_path_ext/binary >>,

    Token_string =
        erlang:list_to_binary(
            [ io_lib:format("~2.16.0b", [X]) || <<X>> <=  Path_binary]
        ),
    Token_long =
        erlang:list_to_integer(erlang:binary_to_list(Token_string), 16),
    Md5_string =
        erlang:list_to_binary(
            [ io_lib:format("~2.16.0b", [X]) || <<X>> <=  Md5_binary]
        ),
    Md5_long =
        erlang:list_to_integer(erlang:binary_to_list(Md5_string), 16),

    empdb_dao:with_transaction(fun(Con)->
        %% -------------------------------------------------------------------
        %% Выбираем тип файла.
        %% -------------------------------------------------------------------
        {ok, [{Filetypepl}]} = empdb_dao_filetype:get(Con, [
            {ext, Ul_ext},
            {limit, 1}
        ]),
        Filetype_id = proplists:get_value(id, Filetypepl),

        %% -------------------------------------------------------------------
        %% Создаем контейнер, куда помещаем описания файлов
        %% -------------------------------------------------------------------
        {ok, [{File}]} = empdb_dao_file:create(Con, [
            {doc_id,            Doc_id},
            {owner_id,          Owner_id},
            {tokenstring,       Token_string},
            {tokenlong,         Token_long}
            
%             {dlfileinfo_id,     proplists:get_value(id, Dlfileinfopl)},
%             {fsfileinfo_id,     proplists:get_value(id, Fsfileinfopl)},
%             {ulfileinfo_id,     proplists:get_value(id, Ul_fileinfopl)}
        ]),

        File_id = proplists:get_value(id, File),

        %% -------------------------------------------------------------------
        %% Создаем описание файла, который загрузил пользователь.
        %% -------------------------------------------------------------------
        {ok, [{Ul_fileinfopl}]} = empdb_dao_fileinfo:create(Con, [
            {fileinfotype_alias,    upload},
            {doc_id,                Doc_id},
            {owner_id,              Owner_id},
            {filetype_id,           Filetype_id},
            {'size',                Ul_size},
            {tokenstring,           Token_string},
            {tokenlong,             Token_long},
            {md5string,             Md5_string},
            {md5long,               Md5_long},
            {file_id,               File_id},
            {name,                  Ul_name}
        ]),

        %% -------------------------------------------------------------------
        %% Создаем описание файла, 
        %% который собираемся хранить в файловой системе
        %% -------------------------------------------------------------------
        {ok, [{Fsfileinfopl}]} = empdb_dao_fileinfo:create(Con, [
            {fileinfotype_alias,    filesystem},
            {doc_id,                Doc_id},
            {owner_id,              Owner_id},
            {filetype_id,           Filetype_id},
            {'size',                Ul_size},
            {tokenstring,           Token_string},
            {tokenlong,             Token_long},
            {md5string,             Md5_string},
            {md5long,               Md5_long},
            {dir,                   Fs_dir},
            {file_id,               File_id},
            {path,                  Fs_path_ext}
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
            {'size',                Ul_size},
            {tokenstring,           Token_string},
            {tokenlong,             Token_long},
            {md5string,             Md5_string},
            {md5long,               Md5_long},
            {dir,                   Dl_dir},
            {file_id,               File_id},
            {path,                  Dl_path_ext}
        ]),

        {ok, [{[
            {file_id,       File_id},
            {originalname,  Ul_name},
            {path,          Dl_path_full},
            {md5sum,        Md5_string}
        ]}]}
    end).

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