-ifndef(__NORM_494696146__).
-define(__NORM_494696146__, true).

-define(NORM_CONVERTER, norm_types).
-define(UNIQ_UNDEFINED, 'norm:undefined').

-record(norm, {
    return=[] :: [{atom(), any()}],
    errors=[] :: [{atom(), any()}]
}).

-type norm_key()    :: atom() | binary() .
-type norm_type()   :: atom()|fun((any()) -> any()).

-record(norm_rule, {
    key =          ?UNIQ_UNDEFINED  ::  norm_key() | [norm_key()],
    nkey =         ?UNIQ_UNDEFINED  ::  norm_key() | [norm_key()],
    types =        []               ::  [norm_type()] ,
    required    = true,
    default =      ?UNIQ_UNDEFINED  ::  any()
}).

-record(norm_error, {
    reason =        ?UNIQ_UNDEFINED :: param | types,
    value =         ?UNIQ_UNDEFINED :: any(),
    rule =          #norm_rule{}    :: record(norm_rule)
}).


-endif. %%% __NORM_494696146__


