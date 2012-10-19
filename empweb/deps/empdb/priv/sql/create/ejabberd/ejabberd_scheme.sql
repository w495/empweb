--
-- PostgreSQL database dump
--


create or replace function utcnow() returns timestamp as $$
        select now() at time zone 'UTC'
$$ language sql;


CREATE TABLE irc_custom (
    jid text NOT NULL,
    host text NOT NULL,
    data text NOT NULL,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);

--

CREATE TABLE last (
    username text NOT NULL,
    seconds text NOT NULL,
    state text NOT NULL
);


CREATE TABLE motd (
    username text NOT NULL,
    xml text,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);


CREATE TABLE muc_registered (
    jid text NOT NULL,
    host text NOT NULL,
    nick text NOT NULL,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);


CREATE TABLE muc_room (
    name text NOT NULL,
    host text NOT NULL,
    opts text NOT NULL,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);


CREATE TABLE privacy_default_list (
    username text NOT NULL,
    name text NOT NULL
);


CREATE TABLE privacy_list (
    username text NOT NULL,
    name text NOT NULL,
    id integer NOT NULL,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);


CREATE TABLE privacy_list_data (
    id bigint,
    t character(1) NOT NULL,
    value text NOT NULL,
    action character(1) NOT NULL,
    ord numeric NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL
);


CREATE SEQUENCE privacy_list_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER SEQUENCE privacy_list_id_seq OWNED BY privacy_list.id;



SELECT pg_catalog.setval('privacy_list_id_seq', 1, false);


CREATE TABLE private_storage (
    username text NOT NULL,
    namespace text NOT NULL,
    data text NOT NULL,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);



CREATE TABLE pubsub_item (
    nodeid bigint,
    itemid text,
    publisher text,
    creation text,
    modification text,
    payload text
);


CREATE TABLE pubsub_node (
    host text,
    node text,
    parent text,
    type text,
    nodeid integer NOT NULL
);


CREATE SEQUENCE pubsub_node_nodeid_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER SEQUENCE pubsub_node_nodeid_seq OWNED BY pubsub_node.nodeid;



SELECT pg_catalog.setval('pubsub_node_nodeid_seq', 1, false);


CREATE TABLE pubsub_node_option (
    nodeid bigint,
    name text,
    val text
);


CREATE TABLE pubsub_node_owner (
    nodeid bigint,
    owner text
);

CREATE TABLE pubsub_state (
    nodeid bigint,
    jid text,
    affiliation character(1),
    subscriptions text,
    stateid integer NOT NULL
);



CREATE SEQUENCE pubsub_state_stateid_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;



ALTER SEQUENCE pubsub_state_stateid_seq OWNED BY pubsub_state.stateid;



SELECT pg_catalog.setval('pubsub_state_stateid_seq', 1, false);


CREATE TABLE pubsub_subscription_opt (
    subid text,
    opt_name character varying(32),
    opt_value text
);



CREATE TABLE roster_version (
    username text NOT NULL,
    version text NOT NULL
);


CREATE TABLE rostergroups (
    username text NOT NULL,
    jid text NOT NULL,
    grp text NOT NULL
);



CREATE TABLE rosterusers (
    username text NOT NULL,
    jid text NOT NULL,
    nick text NOT NULL,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    askmessage text NOT NULL,
    server character(1) NOT NULL,
    subscribe text,
    type text,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);



CREATE TABLE spool (
    username text NOT NULL,
    xml text NOT NULL,
    seq integer NOT NULL,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);



CREATE SEQUENCE spool_seq_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;



ALTER SEQUENCE spool_seq_seq OWNED BY spool.seq;



SELECT pg_catalog.setval('spool_seq_seq', 1, false);


CREATE TABLE sr_group (
    name text NOT NULL,
    opts text NOT NULL,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);



CREATE TABLE sr_user (
    jid text NOT NULL,
    grp text NOT NULL,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);

CREATE TABLE users (
    username text NOT NULL,
    password text NOT NULL,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);



CREATE TABLE vcard (
    username text NOT NULL,
    vcard text NOT NULL,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);


CREATE TABLE vcard_search (
    username text NOT NULL,
    lusername text NOT NULL,
    fn text NOT NULL,
    lfn text NOT NULL,
    family text NOT NULL,
    lfamily text NOT NULL,
    given text NOT NULL,
    lgiven text NOT NULL,
    middle text NOT NULL,
    lmiddle text NOT NULL,
    nickname text NOT NULL,
    lnickname text NOT NULL,
    bday text NOT NULL,
    lbday text NOT NULL,
    ctry text NOT NULL,
    lctry text NOT NULL,
    locality text NOT NULL,
    llocality text NOT NULL,
    email text NOT NULL,
    lemail text NOT NULL,
    orgname text NOT NULL,
    lorgname text NOT NULL,
    orgunit text NOT NULL,
    lorgunit text NOT NULL
);


CREATE TABLE vcard_xupdate (
    username text NOT NULL,
    hash text NOT NULL,
    created_at timestamp without time zone DEFAULT utcnow() NOT NULL
);

ALTER TABLE ONLY privacy_list ALTER COLUMN id SET DEFAULT nextval('privacy_list_id_seq'::regclass);


ALTER TABLE ONLY pubsub_node ALTER COLUMN nodeid SET DEFAULT nextval('pubsub_node_nodeid_seq'::regclass);


ALTER TABLE ONLY pubsub_state ALTER COLUMN stateid SET DEFAULT nextval('pubsub_state_stateid_seq'::regclass);


ALTER TABLE ONLY spool ALTER COLUMN seq SET DEFAULT nextval('spool_seq_seq'::regclass);


ALTER TABLE ONLY last
    ADD CONSTRAINT last_pkey PRIMARY KEY (username);

ALTER TABLE ONLY motd
    ADD CONSTRAINT motd_pkey PRIMARY KEY (username);


ALTER TABLE ONLY privacy_default_list
    ADD CONSTRAINT privacy_default_list_pkey PRIMARY KEY (username);


ALTER TABLE ONLY privacy_list
    ADD CONSTRAINT privacy_list_id_key UNIQUE (id);


ALTER TABLE ONLY pubsub_node
    ADD CONSTRAINT pubsub_node_nodeid_key UNIQUE (nodeid);


ALTER TABLE ONLY pubsub_state
    ADD CONSTRAINT pubsub_state_stateid_key UNIQUE (stateid);


ALTER TABLE ONLY roster_version
    ADD CONSTRAINT roster_version_pkey PRIMARY KEY (username);

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (username);

ALTER TABLE ONLY vcard
    ADD CONSTRAINT vcard_pkey PRIMARY KEY (username);

ALTER TABLE ONLY vcard_search
    ADD CONSTRAINT vcard_search_pkey PRIMARY KEY (lusername);

ALTER TABLE ONLY vcard_xupdate
    ADD CONSTRAINT vcard_xupdate_pkey PRIMARY KEY (username);

ALTER TABLE ONLY privacy_list_data
    ADD CONSTRAINT privacy_list_data_id_fkey FOREIGN KEY (id) REFERENCES privacy_list(id) ON DELETE CASCADE;

ALTER TABLE ONLY pubsub_item
    ADD CONSTRAINT pubsub_item_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES pubsub_node(nodeid) ON DELETE CASCADE;

ALTER TABLE ONLY pubsub_node_option
    ADD CONSTRAINT pubsub_node_option_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES pubsub_node(nodeid) ON DELETE CASCADE;

ALTER TABLE ONLY pubsub_node_owner
    ADD CONSTRAINT pubsub_node_owner_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES pubsub_node(nodeid) ON DELETE CASCADE;

ALTER TABLE ONLY pubsub_state
    ADD CONSTRAINT pubsub_state_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES pubsub_node(nodeid) ON DELETE CASCADE;
