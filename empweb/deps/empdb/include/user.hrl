-ifndef(__USER_024295106__).
-define(__USER_024295106__, true).

-type id()      :: integer().
-type text()   :: binary().

-record(user, {
    id          :: id(), 
    name        :: text(),
    description :: text(), 
    nick        :: text(),
    pass        :: text(),
    phash       :: text(),
    email       :: text(),
    phone       :: text(),
    fname       :: text(),
    sname       :: text(),
    birthday    :: {{},{}},  %% date
    city        :: text(),
    married     :: integer(),
    emotion_id  :: integer(),
    money       :: float(),
    online      :: true | false ,
    country_id  %% int
}).



-endif. %%% __USER_024295106__


