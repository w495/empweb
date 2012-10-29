-ifndef(__EMPWEB_BIZ_SESSION_122586470__).
-define(__EMPWEB_BIZ_SESSION_122586470__, true).

%%% ---------------------------------------------------------------------------
%%% НАСТРОЙКИ СЕСИИ
%%% ---------------------------------------------------------------------------


-record(empweb_biz_session, {
    uid,
    id,
    login,
    perm_names=[],
    time,
    phash
}).


% -define(AUTHCOOKIE, config:get(cookiename, "MCHS")).
% -define(EXPCOOKIE, config:get(expcookie, 18000)).
% 
% 
% -define(F_COOKIEOPTIONS, [{max_age, ?EXPCOOKIE}, {path, "/"}]).
% 
% -define(CATCHA_COOKIE, "captcha_codehex").


-endif. %%% __EMPWEB_BIZ_SESSION_122586470__
