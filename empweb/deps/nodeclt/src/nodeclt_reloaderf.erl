-module(nodeclt_reloaderf).

-export([reload_code/0]).

reload_code() ->
    try
      Reload = fun(M) ->
                   code:purge(M),
                   code:soft_purge(M),
                   {module, M} = code:load_file(M),
                   {ok,M}
               end,
%      Modules = [M || {M,P} <- code:all_loaded(),
%                               is_list(P) andalso
%                               string:str(P, filename:absname(""))>0],
      Modules = modified_modules(),
      {ok, lists:sort([Reload(M) || M <- Modules])}
    catch
  Cls:Why -> {error, {Cls,Why}}
    end.

modified_modules() ->
    [M || {M, P} <- code:all_loaded(),
                    is_list(P),
                    string:str(P, filename:absname(""))>0,
                    module_modified(M) == true].

module_modified(Module) ->
    case code:is_loaded(Module) of
  {file, preloaded} ->
      false;
  {file, Path} ->
      Compile_opts = proplists:get_value(compile, Module:module_info()),
      Compile_time = proplists:get_value(time, Compile_opts),
      Src = proplists:get_value(source, Compile_opts),
      module_modified(Path, Compile_time, Src);
  _ ->
      false
    end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
  false ->
      false;
  Mod_path ->
      {ok, {_, [{_, CB}]}} = beam_lib:chunks(Mod_path, ["CInf"]),
      Compile_opts = binary_to_term(CB),
      Compile_time = proplists:get_value(time, Compile_opts),
      Src = proplists:get_value(source, Compile_opts),
      not ((Compile_time == PrevCompileTime) and (Src == PrevSrc))
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
  {ok, _} ->
      Path;
  _ ->
      %% may be the path was changed?
      case code:where_is_file(filename:basename(Path)) of
    non_existing ->
        false;
    NewPath ->
        NewPath
      end
    end.
