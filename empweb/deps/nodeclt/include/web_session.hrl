-define(SESSION_TABLE_NAME, list_to_atom(atom_to_list(node()) ++ "_session")).

-record(web_session, {
    uid,
    login,
    customer_id,
    permissions=[],
    time,
    password_hash
}).
