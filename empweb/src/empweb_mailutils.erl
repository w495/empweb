%%% @file empweb_mailutils.erl
%%%
%%%     Вспомогательные контроллеры, отправки почты
%%%

-module(empweb_mailutils).

-export([
    mail/3,
    mail/4,
    test_mail/0,
    test/0,
    mmh_person/2,
    mmh_utf8/1,
    test/1
]).
% 
% -include("common.hrl").
% -include("web.hrl").


-define(D(X),           io:format(X)).
-define(D(X, Y),        io:format(X, Y)).

-define(I(X),           io:format(X)).
-define(I(X, Y),        io:format(X, Y)).


-define(E(X),           io:format(X)).
-define(E(X, Y),        io:format(X, Y)).

-define(SYS_DNS,        "SYS_DNS").


-define(DEFAULT_SYS_MAIL_NAME,      <<"Empire">>).
-define(DEFAULT_SYS_MAIL_USERNAME,  <<"nikitin.i@tvzavr.ru">>).
-define(DEFAULT_SYS_MAIL_RELAY,     <<"active-video.ru">>).
-define(DEFAULT_SYS_MAIL_PASSWORD,  <<"maiqu6Ce6aht">>).

-define(CONTACT_EMAIL_ADDRESS,      config_get(contact_email,       ?DEFAULT_SYS_MAIL_USERNAME)).
-define(CONTACT_EMAIL_NAME,         config_get(contact_name,       ?DEFAULT_SYS_MAIL_NAME)).


%%
%% Нижеследующие строки вообще нужны?
%%
-define(SYS_MAIL_NAME,      config_get(contact_name,       ?DEFAULT_SYS_MAIL_NAME)).
-define(SYS_MAIL_USERNAME,  config_get(contact_email,   ?DEFAULT_SYS_MAIL_USERNAME)).
-define(SYS_MAIL_RELAY,     config_get(sys_mail_relay,      ?DEFAULT_SYS_MAIL_RELAY)).
-define(SYS_MAIL_PASSWORD,  config_get(sys_mail_password,   ?DEFAULT_SYS_MAIL_PASSWORD)).

-define(SYS_MAIL_OPTIONS,
    [
        {relay,    ?SYS_MAIL_RELAY},
        {username, ?SYS_MAIL_USERNAME},
        {password, ?SYS_MAIL_PASSWORD}
    ]).


config_get(Some, Def) ->
    case application:get_application() of
        {ok, App} ->
            case application:get_env(Some, App) of
                {ok, Res} ->
                    Res;
                _ ->
                    Def
            end;
        _ ->
            Def
    end.

%%%
%%% @doc
%%%    Формирует строку для заголовка почты в формате utf8
%%%
mmh_utf8(In)->
    erlang:list_to_binary([<<"=?UTF-8?B?">>, base64:encode(In), <<"?=">>]).

%%%
%%% @doc
%%%     Формирует значение заголовка
%%%     отправителя \ получателя почты в формате utf8
%%%     Если имя и адрес не указаны,
%%%     используются значения для системы.
%%%     Т.е если пользователь в какой-то момень отказался без почты,
%%%     то система отпраляет почту сама себе.
%%%
mmh_person(null, null)->
    mmh_person(?SYS_MAIL_NAME, ?SYS_MAIL_USERNAME);

mmh_person(null, Mail)->
    mmh_person(?SYS_MAIL_NAME, Mail);

mmh_person(Name, null)->
    mmh_person(Name, ?SYS_MAIL_USERNAME);

mmh_person(Aname, Amail)->
    Name = mmh_utf8(erlang:list_to_binary([Aname])),
    Mail = erlang:list_to_binary([Amail]),

    X = <<Name/binary," <",Mail/binary,">">>.

%%%
%%% @doc
%%%    Обертка mail/4
%%%
mail({Rmail, Rname}, Rsubject, Rbody) ->
    mail(Rmail, Rname, Rsubject, Rbody);

%%%
%%% @doc
%%%    Отправляет почту нескольким получателям
%%%
mail([], Rsubject, Rbody) -> ok;
mail([{Rmail, Rname} | Rest ], Rsubject, Rbody) ->
    mail(Rmail, Rname, Rsubject, Rbody),
    mail(Rest, Rsubject, Rbody).

%%%
%%% @doc
%%%    Отправляет почту одному получателю
%%%
mail(null, Rname, Rsubject, Rbody) ->
    mail(?SYS_MAIL_USERNAME, Rname, Rsubject, Rbody);

mail(Rmail, Rname, Rsubject, Rbody)
        when erlang:is_list(Rbody) ->
    mail(Rmail, Rname, Rsubject, erlang:list_to_binary(Rbody));

mail(Rmail, Rname, Rsubject, Rbody)
        when erlang:is_binary(Rbody) ->
    ?D("~n mail goes to send (mail = ~p, name = ~p, subject = ~p)~n",
        [Rmail, Rname, base64:encode_to_string(Rsubject)]),
    Email = {<<"text">>, <<"plain">>,
            [
                {<<"From">>,    mmh_person(?SYS_MAIL_NAME, ?SYS_MAIL_USERNAME)},
                {<<"To">>,      mmh_person(Rname, Rmail)},
                {<<"Subject">>, mmh_utf8(Rsubject)
                }
            ], [], Rbody},
    Ans = gen_smtp_client:send(
        {
            ?SYS_MAIL_USERNAME,
            [Rmail],
            mimemail:encode(Email)
        },  ?SYS_MAIL_OPTIONS
    ),
    ?I("~n mail was send (mail = ~p, name = ~p, subject = ~p)~n",
        [Rmail, Rname, base64:encode_to_string(Rsubject)]),
    ?D("~n~n~p~n~n~n", [Email]),
    Ans;

mail(Rmail, Rname, Rsubject, Rbody) ->
    mail(Rmail, Rname, Rsubject, empweb_convert:to_binary(Rbody)).

test_mail() ->

    Rmail = "countff@gmail.com",
    Rname = "Получатель",
    Rsubject = "Тема письма",
%    Xslb_path = "xsl/mail/outside/test_mailb.xsl",
    Xslb_path = "xsl/mail/outside/mkbill.xsl",

%    Xmlb  = xml:encode_data([{"meta",[{"current-path","sd"}]}]),
    Xmlb  = xml:encode_data(
        [
            {"meta",
                [
                    {"sys-dns",     ?SYS_DNS},
                    {"usermail",    Rmail},
                    {"username",    Rname}
                ]
            },
            {"video", "JJJJJJJ"}
        ]
    ),
    Rbody = xslt:apply(Xslb_path, Xmlb),

    mail(Rmail, Rname, Rsubject, Rbody).


test()->
    ok.

test(speed) ->
    ok.