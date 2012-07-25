-include("web_session.hrl").

-define(GLOBAL_TABLES,[]).

-define(LOCAL_TABLES,
        [
            {?SESSION_TABLE_NAME, [{disc_copies, [node()]},
                {local_content, true},
                {record_name, web_session},
                {attributes, record_info(fields, web_session)}]}
        ]
).

