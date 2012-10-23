-ifndef(__EMPWEB_BIZ_SESSION_122586470__).
-define(__EMPWEB_BIZ_SESSION_122586470__, true).

%%% ---------------------------------------------------------------------------
%%% НАСТРОЙКИ СЕСИИ
%%% ---------------------------------------------------------------------------

-define(EMPWEB_BIZ_SESSION_TABLENAME,
    list_to_atom(atom_to_list(node()) ++ "_session")
).

-record(biz_session, {
    uid,
    id,
    login,
    perm_names=[],
    time,
    phash
}).

-define(EMPWEB_BIZ_SESSION_EXPIRETIMEOUT, 18000).

% -define(AUTHCOOKIE, config:get(cookiename, "MCHS")).
% -define(EXPCOOKIE, config:get(expcookie, 18000)).
% 
% 
% -define(F_COOKIEOPTIONS, [{max_age, ?EXPCOOKIE}, {path, "/"}]).
% 
% -define(CATCHA_COOKIE, "captcha_codehex").


-endif. %%% __EMPWEB_BIZ_SESSION_122586470__
