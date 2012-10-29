%%
%%  @file empdb_lgps.erl  Генератор запоминающихся случайных
%%                  последовательностей символов, например, капчи или паролей
%%

-module(empdb_lgps).

-export([
    new/0,  %% Создает последовательность символов по умолчанию.
    new/1,  %% Создает последовательность символов, по описанию (см ниже).
    osn/2,  %% Формирует список n-грамм длины 0 < Len < 13.
    test/0, %% Тестирование.
    test/1  %% Нагрузочное тестирование.
]).

%%
%% Некоторый текст на английском языке
%%
-define(TEXT,
    "tipsforhiringanallstarsupportteamthisarticl"
    "eshowsyouhowtobuildasustainableprocessforwe"
    "hiringtheverybestpeopleforyoursupportteamle"
    "arnhowtoshiftyourfocusfrompedigreetoaptitud"
    "ethreegoalsforscreeningcandidatesoverthepho"
    "newhyyourteamshouldparticipateintheintervie"
    "wprocessandmoremakebetterbusinessdecisionsw"
    "ithflexibleautomatedbusinessrulesthisarticl"
    "eshowsyouhowtobuildasustainableprocessforhi"
    "ringtheverybestpeopleforyoursupportteamlear"
    "nhowtoshiftyourfocusfrompedigreetoaptitudet"
    "hreegoalsforscreeningcandidatesoverthephone"
    "whyyourteamshouldparticipateintheinterviewp"
    "rocessandmoretipsforhiringanallstarsupporto"
    "contentsetymologyhistoryearlyperiodskievanr"
    "usgrandduchyofmoscowtsardomofrussiaimperial"
    "russiasovietrussiarussianfederationpolitics"
    "foreignrelationsmilitarypoliticaldivisionsg"
    "eographytopographyclimatebiodiversityeconom"
    "yagricultureenergytransportscienceandtechno"
    "logydemographicslargestcitieslanguagereligi"
    "onhealtheducationculturefolkcultureandcuisi"
    "nearchitecturevisualartsmusicanddancelitera"
    "tureandphilosophycinemawanimationandmediasp"
    "ortsnationalholidaysandsymbolstourismseeals"
    "oreferencesfurtherreadingexternallinkslinks"
    "understandingofthescenecontentofavideoseque"
    "nceisveryimportantforcontentbasedindexingan"
    "isareainthepastseveralyearshasfocusedontheu"
    "seofspeechrecognitionandimageanalysistechni"
    "quesasacomplimentaryefforttothepriorworkweh"
    "avefocusedonusingtheassociatedaudioinformat"
    "ionmainlythenonspeechportionforvideoscenean"
    "riminatingfivetypesoftvprogramsnamelycommer"
    "cialsbasketballgamesfootballgamesnewsreport"
    "sandweatherforecastsasetoflowlevelaudiofeat"
    "uresareproposedforcharacterizingsemanticcon"
    "tentsofshortaudioclipsthelinearseparability"
    "aceisexaminedusingaclusteringanalysistheeff"
    "ectivefeaturesareidentifiedbyevaluatingthei"
    "ntraclusterandinterclusterscatteringmatrice"
    "softhefeaturespaceusingthesefeaturesaneural"
    "netclassifierwassuccessfulinseparatingtheab"
    "ovefivetypesoftvprogramsbyevaluatingthechan"
    "gesbetweenthefeaturevectorsofadjacentclipsa"
    "toperformsceneanalysisinavideosequenceoneco"
    "mmonapproachistofirstsegmentthesequenceinto"
    "shotssothateachshotcontainsthesametypeofsce"
    "neandthenclassifyeachshotintoonescenetypeus"
    "uallyscenesegmentationisaccomplishedbydetec"
    "tingsignificantchangesinthestatisticsoftheu"
    "nderlyingvisualandaudiosignalsinthissection"
    "weconsiderscenesegmentationbasedonaudioinfo"
    "rmationonlynotethatanaudiosegmentbelongingt"
    "shotsforexampleifinthemiddleofacommercialth"
    "ebackgroundmusicischangedthenthesequencemay"
    "besegmentedintotwoshotsalthoughtheymaybothb"
    "eclassifiedascommercialinalaterclassificati"
    "onstagespeechsegmentationisafundamentalproc"
    "essinginspeechrecognitionwhereaspeechsignal"
    "issegmentedintopiecescontainingvoiceunvoice"
    "andsilencesegmentationoftheaudiosignalisqui"
    "tedifferentfromthatofpurespeechinspeechthel"
    "engthofeachsegmentisveryshortandtheonsetand"
    "offsetofthesegmentshouldbepreciselydetermin"
    "edontheotherhandinourtaskwewanttotrackthese"
    "manticcontentofanaudiosequencenormallytheau"
    "diosignalwiththesamescenecontentwilllastfro"
    "mseveralsecondstoseveralminutesbecauseascen"
    "etransitionusuallyoccursoverarelativelylong"
    "periodsothatonescenegraduallychangestoanoth"
    "erexactlocalizationofthetransitiontimeisdif"
    "ficulttoachieveandisusuallynotnecessaryitis"
    "sufficientformostpracticalapplicationsifatr"
    "ansitionisdetectedshortlyafterthenewsceneis"
    "stabilizedperiodsothatonescenegraduallythan"
).

%%
%% Набор слогов, некоторые сочетания были убраны,
%% как неблагозвучные с точки зрения русского млм английского языка.
%%
-define(SYLLABLES_L,[
    "ba","ca","da","fa","ga","ma","na","pa","qa","ra","sa","ta","va","za",
    "bi","ci","di","fi","gi","mi","ni","pi","qi","ri","si","ti","vi","zi",
    "be","ce","de","fe","ge","me","ne","pe","qe","re","se","te","ve","ze",
    "by","cy","dy","fy","gy","my","ny","py","qy","ry","sy","ty","vy","zy",
    "bo","co","do","fo","go","mo","no","po","qo","ro","so","to","vo","zo",
    "bu","cu","du","fu","gu","mu","nu","pu","qu","ru","su","tu","vu","zu"
]).

%%
%% Набор слогов, c заменой первой буквы на прописную.
%%
-define(SYLLABLES_C,[
    "Ba","Ca","Da","Fa","Ga","Ma","Na","Pa","Qa","Ra","Sa","Ta","Va","Za",
    "Bi","Ci","Di","Fi","Gi","Mi","Ni","Pi","Qi","Ri","Si","Ti","Vi","Zi",
    "Be","Ce","De","Fe","Ge","Me","Ne","Pe","Qe","Re","Se","Te","Ve","Ze",
    "By","Cy","Dy","Fy","Gy","My","Ny","Py","Qy","Ry","Sy","Ty","Vy","Zy",
    "Bo","Co","Do","Fo","Go","Mo","No","Po","Qo","Ro","So","To","Vo","Zo",
    "Bu","Cu","Du","Fu","Gu","Mu","Nu","Pu","Qu","Ru","Su","Tu","Vu","Zu"
]).

%%
%% Набор строчных букв, некоторые были убраны,
%% т.к. их можно перепутать с другими при создании капчи.
%%
-define(LETERS_L,
    "abcdefghjkmnpqrstuvwxyz"
).

%%
%% Набор прописных букв, некоторые были убраны,
%% т.к. их можно перепутать с другими при создании капчи.
%%
-define(LETERS_C,
    "ABCDEFGHJKLMNPQRSTUVWXYZ"
).

%% --------------------------------------------------------------------------
%% Внешние функции
%% --------------------------------------------------------------------------


%%
%% Создает последовательность символов по умолчанию.
%%
-spec   new() -> {ok, binary()}.

%%
%% Создает последовательность символов, согласно описанию.
%%
-spec   new({ngram,         {Plen_1::integer(), Offset_1::integer()},
                            {Plen_2::integer(), Offset_2::integer()}    }) ->
            {ok, binary()};
            %%  Выходной набор коструируется из двух последовательностей.
            %%  Каждая последовательность имеет длинну не менне Plen_i,
            %%  и не более Plen_i, Offset_i, конкретная длина
            %%  выбирается случайно.
        ({ngram,         Plen::integer(), Ngram_nb::integer()}) ->
            {ok, binary()};
            %%  Выдается последовательность из n-грамм длины Plen.
            %%  Состоит из Ngram_nb n-грамм
        ({ngram,         Plen::integer()}) ->
            {ok, binary()};
            %%  Выдается последовательность из одной n-граммы длины Plen.
        ({syllable,      Plen::integer()}) ->
            {ok, binary()};
            %%  Выдается последовательность строчных слогов длины Plen.
        ({'Syllable',    Plen::integer()}) ->
            {ok, binary()};
            %%  Выдается последовательность строчных и прописных
            %%  слогов длины Plen.
        ({leter,         Plen::integer()}) ->
            {ok, binary()}.
            %%  Выдается последовательность строчных и прописных
            %%  букв длины Plen.

%%
%% Формирует список n-грамм длины Len.
%% Len не более 13.
%%
-spec   osn(binary(),   Len::integer())  -> [binary()];
           (list(),     Len::integer())    -> [list()].

%%
%% Производит тестирование модуля
%%
-spec   test() -> ok.

%% 
%% Производит нагрузочное тестирование модуля.
%% 
-spec   test(speed) -> ok.

%% 
%% @spec new() -> {ok, binary()}
%% @doc Создает последовательность символов по умолчанию.
%%
new() ->
    new({ngram, {3, 2}, {4, 3}}).

%%
%% @spec new(
%%     {ngram,          {integer(), integer()}, {integer(), integer()}}
%%     | {ngram,        integer(), integer()}
%%     | {ngram,        integer()}
%%     | {syllable,     integer()}
%%     | {'Syllable',   integer()}
%%     | {leter,        integer()}
%% ) -> {ok, binary()}
%% @doc Создает последовательность символов, согласно Opt описанию:
%%      Opt ::=
%%          {ngram, {Plen_1, Offset_1}, {Plen_2, Offset_2}}
%%              Выходной набор коструируется из двух последовательностей.
%%              Каждая последовательность имеет длинну не менне Plen_i,
%%              и не более Plen_i, Offset_i, конкретная длина
%%              выбирается случайно.
%%          {ngram, Plen, Ngram_nb}
%%              Выдается последовательность из n-грамм длины Plen.
%%              Состоит из Ngram_nb n-грамм
%%          {ngram, Plen}
%%              Выдается последовательность из одной n-граммы длины Plen.
%%          {syllable, Plen}
%%              Выдается последовательность строчных слогов длины Plen.
%%          {'Syllable', Plen}
%%              Выдается последовательность строчных и прописных
%%              слогов длины Plen.
%%          {leter, Plen}
%%              Выдается последовательность строчных и прописных
%%              букв длины Plen.
%%
new(Opt) ->
    Code = generate_rand(Opt),
    {ok, Code}.

%%
%% @spec osn(binary()|list(),   Len::integer())  -> [binary()];
%% @doc Формирует список n-грамм длинны Len.
%%      Len не более 13.
%%
osn(<<_, R/binary>>, 0) ->
    osn(R, 0);
osn(<<H1, R/binary>>, 1) ->
    [<<H1>> | osn(R, 1)];
osn(<<H1, H2, R/binary>>, 2) ->
    [<<H1, H2>> | osn(<<H2, R/binary>>, 2)];
osn(<<H1, H2, H3, R/binary>>, 3) ->
    [<<H1, H2, H3>> | osn(<<H2, H3, R/binary>>, 3)];
osn(<<H1, H2, H3, H4, R/binary>>, 4) ->
    [<<H1, H2, H3, H4>> | osn(<<H2, H3, H4, R/binary>>, 4)];
osn(<<H1, H2, H3, H4, H5, R/binary>>, 5) ->
    [<<H1, H2, H3, H4, H5>> | osn(<<H2, H3, H4, H5, R/binary>>, 5)];
osn(<<H1, H2, H3, H4, H5, H6, R/binary>>, 6) ->
    [<<H1, H2, H3, H4, H5, H6>> | osn(<<H2, H3, H4, H5, H6, R/binary>>, 6)];
osn(<<H1, H2, H3, H4, H5, H6, H7, R/binary>>, 7) ->
    [<<H1, H2, H3, H4, H5, H6, H7>>
        | osn(<<H2, H3, H4, H5, H6, H7, R/binary>>, 7)];
osn(<<H1, H2, H3, H4, H5, H6, H7, H8, R/binary>>, 8) ->
    [<<H1, H2, H3, H4, H5, H6, H7, H8>>
        | osn(<<H2, H3, H4, H5, H6, H7, H8, R/binary>>, 8)];
osn(<<H1, H2, H3, H4, H5, H6, H7, H8, H9, R/binary>>, 9) ->
    [<<H1, H2, H3, H4, H5, H6, H7, H8, H9>>
        | osn(<<H2, H3, H4, H5, H6, H7, H8, H9, R/binary>>, 9)];
osn(<<H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, R/binary>>, 10) ->
    [<<H1, H2, H3, H4, H5, H6, H7, H8, H9, HA>>
        | osn(<<H2, H3, H4, H5, H6, H7, H8, H9, HA, R/binary>>, 10)];
osn(<<H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, R/binary>>, 11) ->
    [<<H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB>>
        | osn(<<H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, R/binary>>, 11)];
osn(<<H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC, R/binary>>, 12) ->
    [<<H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC>>
        | osn(<<H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC, R/binary>>, 12)];
osn(<<H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC, HD, R/binary>>, 13) ->
    [   <<H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC, HD>>
        | osn(<<H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC, HD, R/binary>>, 13)
    ];
osn([_ | R], 0) ->
    [[] | osn(R, 0)];
osn([H1 | R], 1) ->
    [[H1]
        | osn(R, 1)];
osn([H1, H2 | R], 2) ->
    [[H1, H2]
        | osn([H2 | R], 2)];
osn([H1, H2, H3 | R], 3) ->
    [[H1, H2, H3]
        | osn([H2, H3 | R], 3)];
osn([H1, H2, H3, H4 | R], 4) ->
    [[H1, H2, H3, H4]
        | osn([H2, H3, H4 | R], 4)];
osn([H1, H2, H3, H4, H5 | R], 5) ->
    [[H1, H2, H3, H4, H5]
        | osn([H2, H3, H4, H5 | R], 5)];
osn([H1, H2, H3, H4, H5, H6 | R], 6) ->
    [[H1, H2, H3, H4, H5, H6]
        | osn([H2, H3, H4, H5, H6 | R], 6)];
osn([H1, H2, H3, H4, H5, H6, H7 | R], 7) ->
    [[H1, H2, H3, H4, H5, H6, H7]
        | osn([H2, H3, H4, H5, H6, H7  | R], 7)];
osn([H1, H2, H3, H4, H5, H6, H7, H8 | R], 8) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8 ]
        | osn([H2, H3, H4, H5, H6, H7, H8  | R], 8)];
osn([H1, H2, H3, H4, H5, H6, H7, H8, H9 | R], 9) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9 ]
        | osn([ H2, H3, H4, H5, H6, H7, H8, H9  | R], 9)];
osn([H1, H2, H3, H4, H5, H6, H7, H8, H9, HA | R], 10) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, HA ]
        | osn([H2, H3, H4, H5, H6, H7, H8, H9, HA |R], 10)];
osn([H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB | R], 11) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB ]
        | osn([H2, H3, H4, H5, H6, H7, H8, H9, HA, HB |R], 12)];
osn([H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC | R], 12) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC ]
        | osn([H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC | R], 12)];
osn([H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC, HD | R], 13) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC, HD ]
        | osn([H2, H3, H4, H5, H6, H7, H8, H9, HA, HB, HC, HD | R], 13)];
osn(_,_) -> [].

%%
%% @spec test() -> ok.
%% @doc Производит тестирование модуля.
test()->
    ok.

%%
%% @spec test(speed) -> ok.
%% @doc Производит нагрузочное тестирование модуля.
%%      Сейчас, тестируется скорость выделения n-грамм из списков и binary.
%%      В  данном случае (!) списки оказались быстрее.
test(speed)->
    test_speed(?TEXT).

%% --------------------------------------------------------------------------
%% Локальные функции
%% --------------------------------------------------------------------------

%%
%% @spec generate_rand(
%%     {ngram,          {integer(), integer()}, {integer(), integer()}}
%%     | {ngram,        integer(), integer()}
%%     | {ngram,        integer()}
%%     | {syllable,     integer()}
%%     | {'Syllable',   integer()}
%%     | {leter,        integer()}
%% ) -> binary()
%% @doc Создает последовательность символов, согласно Opt описанию:
%%      Opt ::=
%%          {ngram, {Plen_1, Offset_1}, {Plen_2, Offset_2}}
%%              Выходной набор коструируется из двух последовательностей.
%%              Каждая последовательность имеет длинну не менне Plen_i,
%%              и не более Plen_i, Offset_i, конкретная длина
%%              выбирается случайно.
%%          {ngram, Plen, Ngram_nb}
%%              Выдается последовательность из n-грамм длины Plen.
%%              Состоит из Ngram_nb n-грамм
%%          {ngram, Plen}
%%              Выдается последовательность из одной n-граммы длины Plen.
%%          {syllable, Plen}
%%              Выдается последовательность строчных слогов длины Plen.
%%          {'Syllable', Plen}
%%              Выдается последовательность строчных и прописных
%%              слогов длины Plen.
%%          {leter, Plen}
%%              Выдается последовательность строчных и прописных
%%              букв длины Plen.
%%
generate_rand({ngram, {Plen_1, Offset_1}, {Plen_2, Offset_2}}) ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    List_1 = osn(?TEXT, Plen_1 + (A3 rem Offset_1)),
    List_2 = osn(?TEXT, Plen_2 + (A3 rem Offset_2)),
    erlang:list_to_binary([
        nth(uniform(length(List_1)),List_1),
        nth(uniform(length(List_2)),List_2)
    ]);

generate_rand({ngram, Plen, Ngram_nb}) ->
    get_rpart(osn(?TEXT, Plen), Ngram_nb);
    
generate_rand({ngram, Plen}) ->
    get_rpart(osn(?TEXT, Plen), 1);

generate_rand({syllable, Plen}) ->
    get_rpart(?SYLLABLES_L, Plen);

generate_rand({'Syllable', Plen}) ->
    get_rpart(lists:append(?SYLLABLES_L, ?SYLLABLES_C), Plen);

generate_rand({leter, Plen}) ->
    get_rpart(lists:append(?LETERS_L, ?LETERS_C), Plen).


%%
%% @spec get_rpart(list(), integer()) -> binary()
%% @doc Создает случайную выбоку заданной длины из списка.
%%      Полученная выборка склеивается в binary
%% 
get_rpart(List, Plen) ->
    erlang:list_to_binary([
        nth(X,List)
        ||  X <- lists:map(fun(_)->
            uniform(length(List)) end,
            lists:seq(1, Plen)
        )
    ]).

%%
%% @spec    nth(integer(), []) -> []
%%          nth(integer(), [string()|binary()]) -> string()|binary()
%% @doc Возвращает n-тый эелемент последовательности.
%% 
nth(1, []) ->
    [];
nth(X, Y) ->
    lists:nth(X, Y).

%%
%% @spec uniform(X:integer()) -> integer()
%% @doc Возвращает случайное число не более X.
%% 
uniform(0)->
    1;
uniform(X)->
    random:uniform(X).

%%
%% @spec test_speed(string()) -> ok.
%% @doc Тестирует скорость выделения n-грамм из списков и binary.
%%      В  данном случае (!) списки оказались быстрее.
test_speed(In)->
    test_speed(In, lists),
    test_speed(erlang:list_to_binary(In), binary),
    ok.

%%
%% @spec test_speed(string(), binary|list) -> ok.
%% @doc Тестирует скорость выделения n-грамм из списков и binary,
%%      на 100 операций, всех типов n-грам.
%%      В  данном случае (!) списки оказались быстрее.
%%
test_speed(Ain, binary)->
    io:format("~p~n", [timer:tc(fun(In)->
        lists:foreach(fun(_)->
            osn(In,  1),
            osn(In,  2),
            osn(In,  3),
            osn(In,  4),
            osn(In,  5),
            osn(In,  6),
            osn(In,  7),
            osn(In,  8),
            osn(In,  9),
            osn(In, 10),
            osn(In, 12),
            osn(In, 13)
        end, lists:seq(1, 1000)),
        binary
    end, [Ain])]),
    ok;

test_speed(Ain, lists)->
    io:format("~p~n", [timer:tc(fun(In)->
        lists:foreach(fun(_)->
            osn(In,  1),
            osn(In,  2),
            osn(In,  3),
            osn(In,  4),
            osn(In,  5),
            osn(In,  6),
            osn(In,  7),
            osn(In,  8),
            osn(In,  9),
            osn(In, 10),
            osn(In, 12),
            osn(In, 13)
        end, lists:seq(1, 1000)),
        binary
    end, [Ain])]),
    ok.

