%% @copyright 2013 Empire
%% @author Илья w-495 Никитин
%% @doc Генерации "похожих" строк,
%% используется для предложения новых имен пользователей.
%%

-module(empdb_suggest_old).
-export([
    string/1,
    string/2,
    len/1
]).

string(Orgstring) ->
    string(Orgstring, []).

string(Orgstring, Additions)->
    Maximumnicksize  = proplists:get_value(maximumnicksize, Additions, null),
    {Year,Month,Day} = erlang:date(),

    Leterslist = "etaoinshrdlcumwfgypbvkxjqz",
    Leterslistsize = erlang:length(Leterslist),

    Leters =
        lists:map(
            fun(X)->
                empdb_convert:to_binary([X])
            end,
            Leterslist
        ),

    Digits =
        [
            empdb_convert:to_binary(empdb_convert:to_list(X))
            || X <- lists:seq(0, 9)
        ],

    Random_leters =
        [
            lists:nth(crypto:rand_uniform(1, Leterslistsize), Leters),
            lists:nth(crypto:rand_uniform(1, Leterslistsize), Leters),
            lists:nth(crypto:rand_uniform(1, Leterslistsize), Leters),
            lists:nth(crypto:rand_uniform(1, Leterslistsize), Leters),
            lists:nth(crypto:rand_uniform(1, Leterslistsize), Leters),
            lists:nth(crypto:rand_uniform(1, Leterslistsize), Leters)
        ],

    Random_digits =
        [
            lists:nth(crypto:rand_uniform(1, 10), Digits),
            lists:nth(crypto:rand_uniform(1, 10), Digits),
            lists:nth(crypto:rand_uniform(1, 10), Digits)
        ],


    Seps = lists:usort([
        <<"">>,
%         <<"+">>,
%         <<"$">>,
%         <<"%">>,
%         <<"!">>,
%         <<"*">>,
%         <<"#">>,
%         <<"@">>,
%         <<"&">>,
        <<"_">>,
        <<".">>
        | proplists:get_value(seps, Additions, [])
    ]),


    Prewords = lists:usort([
        <<"re">>,
        <<"my">>,
        <<"co">>,
        <<"to">>,
        <<"top">>,
        <<"sup">>,
        <<"sub">>,
        <<"cool">>
        |
        lists:append([
            Random_leters,
            proplists:get_value(prewords, Additions, [])
        ])
    ]),

    Postwords = lists:usort([
        empdb_convert:to_binary(empdb_convert:to_list(Day)),
        empdb_convert:to_binary(empdb_convert:to_list(Month)),
        empdb_convert:to_binary(empdb_convert:to_list(Year)),
        empdb_convert:to_binary(empdb_convert:to_list(Year - 2000))
        |
        lists:append([
            Random_digits,
            Random_leters,
            proplists:get_value(postwords, Additions, [])
        ])
    ]),

    Stoppunkts = lists:append([
        [
            <<" ">>,
            <<"(">>,
            <<")">>,
            <<"<">>,
            <<">">>,
            <<"[">>,
            <<"]">>,
            <<"{">>,
            <<"}">>,
            <<"-">>,
            <<"+">>,
            <<"$">>,
            <<"%">>,
            <<"^">>,
            <<"!">>,
            <<"*">>,
            <<"~">>,
            <<"`">>,
            <<"'">>,
            <<"\"">>,
            <<"#">>,
            <<"@">>,
            <<"&">>,
            <<"_">>,
            <<".">>,
            <<",">>,
            <<"?">>
        ],
        Seps -- [<<"">>],
        Digits
    ]),

    Stopwords = lists:append([
        %Prewords,
        proplists:get_value(stopwords, Additions, [])
    ]),

    Norgstring =
        binary:part(
            Orgstring,
            0,
            erlang:min(
                len(Orgstring),
                Maximumnicksize
            )
        ),

    Strparts__ = binary:split(Norgstring, Stoppunkts, [global, trim]),

    Strparts_ = lists:foldl(
        fun(Nn, Acc) ->
            lists:append(
                binary:split(
                    Nn,
                    Stopwords,
                    [trim]
                ),
                Acc
            )
        end,
        [],
        Strparts__
    ),

    Strparts =
        case lists:filter(fun(<<>>)-> false; (_)-> true end,  Strparts_) of
            [] ->
                [Norgstring];
            Nres ->
                Nres
        end,

    Sugs_ = lists:append([
        [ empdb_convert:to_binary([Preword, Sep, Str]) ||
            Str <- Strparts,
            Preword <- Prewords,
            Sep <- Seps,
            not string_match(Str, [Preword, Sep]) and
            (Str =/= <<>>)
        ] -- [Orgstring],
        [ empdb_convert:to_binary([Str, Sep, Postword]) ||
            Str <- Strparts,
            Sep <- Seps,
            Postword <- Postwords,
            not string_match(Str, [Sep, Postword])and
            (Str =/= <<>>)
        ] -- [Orgstring]
    ]),

    Sugs =
        sets:to_list(sets:from_list(lists:map(
            fun(Word) ->
                binary:part(Word,0,erlang:min(len(Word), Maximumnicksize))
            end,
            [
                lgps_new({syllable, 2}),
                lgps_new({ngram, 4, 1}),
                lgps_new({syllable, 3}),
                lgps_new({ngram, 6, 1}),
                lgps_new({syllable, 4}),
                lgps_new({ngram, 8, 1})
                |Sugs_
            ]
        ))),

    lists:sort(
        fun(X, Y) ->
            len(X) < len(Y)
        end,
        Sugs
    ).



len(In) ->
    io:format(" ~n~n~n In  = ~p ~n~n~n", [In]),
    Out = ux_string:length(unicode:characters_to_list(In)),
    io:format(" ~n~n~n Out = ~p ~n~n~n", [Out]),
    Out.


string_match(Str, Patterns) ->
    case binary:match(Str, Patterns -- [<<"">>],[]) of
        nomatch ->
            false;
        _ ->
            true
    end.


lgps_new(X) ->
    {ok, Res} = lgps:new(X),
    Res.
