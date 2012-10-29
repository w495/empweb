-ifndef(__LGPS_697606147__).
-define(__LGPS_697606147__, true).

%%
%% Некоторый текст на английском языке
%%
-define(LGPS_TEXT,
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
-define(LGPS_SYLLABLES_L,[
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
-define(LGPS_SYLLABLES_C,[
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
-define(LGPS_LETERS_L,
    "abcdefghjkmnpqrstuvwxyz"
).

%%
%% Набор прописных букв, некоторые были убраны,
%% т.к. их можно перепутать с другими при создании капчи.
%%
-define(LGPS_LETERS_C,
    "ABCDEFGHJKLMNPQRSTUVWXYZ"
).


-endif. %%% __LGPS_697606147__


