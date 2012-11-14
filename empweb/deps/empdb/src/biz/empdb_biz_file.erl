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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Блоги
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % 
% %   empweb_biz_file:create([
% %         {filebody,      Fullbody},
% %         {filename,      Filename},
% %         {nchanks,       Nchanks},
% %         {contenttype,   Contenttype}
% %    ])
% %

create(Params)->
    Fsdir = <<"deps/empdb/priv/data/">>,
    Retdir = <<"/jsonapi/photo/">>,

    Ufname  = proplists:get_value(filename, Params, []),
    Ufbody  = proplists:get_value(filebody, Params, []),
    Ufext   = proplists:get_value(fileextension, Params, []),
    Md5seed = erlang:md5(Ufbody),


    io:format("Ufname = ~p", [Ufname]),

    Md5string =  erlang:list_to_binary(
        [ io_lib:format("~2.16.0b", [X]) || <<X>> <=  Md5seed]
    ),
    
    Pathseed  =
        <<  (crypto:rand_bytes(crypto:rand_uniform(1, 4)))/binary,
            Md5seed/binary,
            (crypto:rand_bytes(crypto:rand_uniform(1, 4)))/binary
        >>,

    Fspath = erlang:list_to_binary(
        filename:join([[ io_lib:format("~.36.0b", [X])] || <<X>> <=  Pathseed])
    ),

    Retpath = erlang:list_to_binary(
        string:join([[ io_lib:format("~.36.0b", [X])] || <<X>> <=  Pathseed], [<<"/">>])
    ),

    Fspathext = << Fspath/binary, $., Ufext/binary  >>,

    Retpathext = << Retpath/binary, $., Ufext/binary  >>,

    Fullretpath =  << Retdir/binary, Retpathext/binary >>,

    Fullfspath =  << Fsdir/binary, Fspathext/binary >>,

    filelib:ensure_dir(Fullfspath),
    file:write_file(Fullfspath, Ufbody),
    
    {ok, [{[
        {originalname,  Ufname},
        {path,          Fullretpath},
        {md5sum,        Md5string}
    ]}]}.

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