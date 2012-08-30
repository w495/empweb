--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: irc_custom; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE irc_custom (
    jid text NOT NULL,
    host text NOT NULL,
    data text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.irc_custom OWNER TO "w-495";

--
-- Name: last; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE last (
    username text NOT NULL,
    seconds text NOT NULL,
    state text NOT NULL
);


ALTER TABLE public.last OWNER TO "w-495";

--
-- Name: motd; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE motd (
    username text NOT NULL,
    xml text,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.motd OWNER TO "w-495";

--
-- Name: muc_registered; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE muc_registered (
    jid text NOT NULL,
    host text NOT NULL,
    nick text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.muc_registered OWNER TO "w-495";

--
-- Name: muc_room; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE muc_room (
    name text NOT NULL,
    host text NOT NULL,
    opts text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.muc_room OWNER TO "w-495";

--
-- Name: privacy_default_list; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE privacy_default_list (
    username text NOT NULL,
    name text NOT NULL
);


ALTER TABLE public.privacy_default_list OWNER TO "w-495";

--
-- Name: privacy_list; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE privacy_list (
    username text NOT NULL,
    name text NOT NULL,
    id integer NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.privacy_list OWNER TO "w-495";

--
-- Name: privacy_list_data; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

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


ALTER TABLE public.privacy_list_data OWNER TO "w-495";

--
-- Name: privacy_list_id_seq; Type: SEQUENCE; Schema: public; Owner: "w-495"
--

CREATE SEQUENCE privacy_list_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.privacy_list_id_seq OWNER TO "w-495";

--
-- Name: privacy_list_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: "w-495"
--

ALTER SEQUENCE privacy_list_id_seq OWNED BY privacy_list.id;


--
-- Name: privacy_list_id_seq; Type: SEQUENCE SET; Schema: public; Owner: "w-495"
--

SELECT pg_catalog.setval('privacy_list_id_seq', 1, false);


--
-- Name: private_storage; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE private_storage (
    username text NOT NULL,
    namespace text NOT NULL,
    data text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.private_storage OWNER TO "w-495";

--
-- Name: pubsub_item; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE pubsub_item (
    nodeid bigint,
    itemid text,
    publisher text,
    creation text,
    modification text,
    payload text
);


ALTER TABLE public.pubsub_item OWNER TO "w-495";

--
-- Name: pubsub_node; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE pubsub_node (
    host text,
    node text,
    parent text,
    type text,
    nodeid integer NOT NULL
);


ALTER TABLE public.pubsub_node OWNER TO "w-495";

--
-- Name: pubsub_node_nodeid_seq; Type: SEQUENCE; Schema: public; Owner: "w-495"
--

CREATE SEQUENCE pubsub_node_nodeid_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.pubsub_node_nodeid_seq OWNER TO "w-495";

--
-- Name: pubsub_node_nodeid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: "w-495"
--

ALTER SEQUENCE pubsub_node_nodeid_seq OWNED BY pubsub_node.nodeid;


--
-- Name: pubsub_node_nodeid_seq; Type: SEQUENCE SET; Schema: public; Owner: "w-495"
--

SELECT pg_catalog.setval('pubsub_node_nodeid_seq', 1, false);


--
-- Name: pubsub_node_option; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE pubsub_node_option (
    nodeid bigint,
    name text,
    val text
);


ALTER TABLE public.pubsub_node_option OWNER TO "w-495";

--
-- Name: pubsub_node_owner; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE pubsub_node_owner (
    nodeid bigint,
    owner text
);


ALTER TABLE public.pubsub_node_owner OWNER TO "w-495";

--
-- Name: pubsub_state; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE pubsub_state (
    nodeid bigint,
    jid text,
    affiliation character(1),
    subscriptions text,
    stateid integer NOT NULL
);


ALTER TABLE public.pubsub_state OWNER TO "w-495";

--
-- Name: pubsub_state_stateid_seq; Type: SEQUENCE; Schema: public; Owner: "w-495"
--

CREATE SEQUENCE pubsub_state_stateid_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.pubsub_state_stateid_seq OWNER TO "w-495";

--
-- Name: pubsub_state_stateid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: "w-495"
--

ALTER SEQUENCE pubsub_state_stateid_seq OWNED BY pubsub_state.stateid;


--
-- Name: pubsub_state_stateid_seq; Type: SEQUENCE SET; Schema: public; Owner: "w-495"
--

SELECT pg_catalog.setval('pubsub_state_stateid_seq', 1, false);


--
-- Name: pubsub_subscription_opt; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE pubsub_subscription_opt (
    subid text,
    opt_name character varying(32),
    opt_value text
);


ALTER TABLE public.pubsub_subscription_opt OWNER TO "w-495";

--
-- Name: roster_version; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE roster_version (
    username text NOT NULL,
    version text NOT NULL
);


ALTER TABLE public.roster_version OWNER TO "w-495";

--
-- Name: rostergroups; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE rostergroups (
    username text NOT NULL,
    jid text NOT NULL,
    grp text NOT NULL
);


ALTER TABLE public.rostergroups OWNER TO "w-495";

--
-- Name: rosterusers; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

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
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.rosterusers OWNER TO "w-495";

--
-- Name: spool; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE spool (
    username text NOT NULL,
    xml text NOT NULL,
    seq integer NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.spool OWNER TO "w-495";

--
-- Name: spool_seq_seq; Type: SEQUENCE; Schema: public; Owner: "w-495"
--

CREATE SEQUENCE spool_seq_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.spool_seq_seq OWNER TO "w-495";

--
-- Name: spool_seq_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: "w-495"
--

ALTER SEQUENCE spool_seq_seq OWNED BY spool.seq;


--
-- Name: spool_seq_seq; Type: SEQUENCE SET; Schema: public; Owner: "w-495"
--

SELECT pg_catalog.setval('spool_seq_seq', 1, false);


--
-- Name: sr_group; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE sr_group (
    name text NOT NULL,
    opts text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.sr_group OWNER TO "w-495";

--
-- Name: sr_user; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE sr_user (
    jid text NOT NULL,
    grp text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.sr_user OWNER TO "w-495";

--
-- Name: users; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE users (
    username text NOT NULL,
    password text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.users OWNER TO "w-495";

--
-- Name: vcard; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE vcard (
    username text NOT NULL,
    vcard text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.vcard OWNER TO "w-495";

--
-- Name: vcard_search; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

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


ALTER TABLE public.vcard_search OWNER TO "w-495";

--
-- Name: vcard_xupdate; Type: TABLE; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE TABLE vcard_xupdate (
    username text NOT NULL,
    hash text NOT NULL,
    created_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.vcard_xupdate OWNER TO "w-495";

--
-- Name: id; Type: DEFAULT; Schema: public; Owner: "w-495"
--

ALTER TABLE ONLY privacy_list ALTER COLUMN id SET DEFAULT nextval('privacy_list_id_seq'::regclass);


--
-- Name: nodeid; Type: DEFAULT; Schema: public; Owner: "w-495"
--

ALTER TABLE ONLY pubsub_node ALTER COLUMN nodeid SET DEFAULT nextval('pubsub_node_nodeid_seq'::regclass);


--
-- Name: stateid; Type: DEFAULT; Schema: public; Owner: "w-495"
--

ALTER TABLE ONLY pubsub_state ALTER COLUMN stateid SET DEFAULT nextval('pubsub_state_stateid_seq'::regclass);


--
-- Name: seq; Type: DEFAULT; Schema: public; Owner: "w-495"
--

ALTER TABLE ONLY spool ALTER COLUMN seq SET DEFAULT nextval('spool_seq_seq'::regclass);


--
-- Data for Name: irc_custom; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY irc_custom (jid, host, data, created_at) FROM stdin;
\.


--
-- Data for Name: last; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY last (username, seconds, state) FROM stdin;
\.


--
-- Data for Name: motd; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY motd (username, xml, created_at) FROM stdin;
\.


--
-- Data for Name: muc_registered; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY muc_registered (jid, host, nick, created_at) FROM stdin;
\.


--
-- Data for Name: muc_room; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY muc_room (name, host, opts, created_at) FROM stdin;
\.


--
-- Data for Name: privacy_default_list; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY privacy_default_list (username, name) FROM stdin;
\.


--
-- Data for Name: privacy_list; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY privacy_list (username, name, id, created_at) FROM stdin;
\.


--
-- Data for Name: privacy_list_data; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY privacy_list_data (id, t, value, action, ord, match_all, match_iq, match_message, match_presence_in, match_presence_out) FROM stdin;
\.


--
-- Data for Name: private_storage; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY private_storage (username, namespace, data, created_at) FROM stdin;
\.


--
-- Data for Name: pubsub_item; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY pubsub_item (nodeid, itemid, publisher, creation, modification, payload) FROM stdin;
\.


--
-- Data for Name: pubsub_node; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY pubsub_node (host, node, parent, type, nodeid) FROM stdin;
\.


--
-- Data for Name: pubsub_node_option; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY pubsub_node_option (nodeid, name, val) FROM stdin;
\.


--
-- Data for Name: pubsub_node_owner; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY pubsub_node_owner (nodeid, owner) FROM stdin;
\.


--
-- Data for Name: pubsub_state; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY pubsub_state (nodeid, jid, affiliation, subscriptions, stateid) FROM stdin;
\.


--
-- Data for Name: pubsub_subscription_opt; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY pubsub_subscription_opt (subid, opt_name, opt_value) FROM stdin;
\.


--
-- Data for Name: roster_version; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY roster_version (username, version) FROM stdin;
\.


--
-- Data for Name: rostergroups; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY rostergroups (username, jid, grp) FROM stdin;
\.


--
-- Data for Name: rosterusers; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY rosterusers (username, jid, nick, subscription, ask, askmessage, server, subscribe, type, created_at) FROM stdin;
\.


--
-- Data for Name: spool; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY spool (username, xml, seq, created_at) FROM stdin;
\.


--
-- Data for Name: sr_group; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY sr_group (name, opts, created_at) FROM stdin;
\.


--
-- Data for Name: sr_user; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY sr_user (jid, grp, created_at) FROM stdin;
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY users (username, password, created_at) FROM stdin;
admin	4rfvbhu8	2012-07-12 16:36:49.23643
michs	4rfvbhu8	2012-07-12 16:43:28.905135
cff	123	2012-07-13 11:42:33.702869
dima	123	2012-07-13 11:42:50.029323
test	123	2012-08-23 11:20:46.66587
sd	sd	2012-08-30 14:36:16.174055
1	sd	2012-08-30 14:36:51.73412
\.


--
-- Data for Name: vcard; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY vcard (username, vcard, created_at) FROM stdin;
\.


--
-- Data for Name: vcard_search; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY vcard_search (username, lusername, fn, lfn, family, lfamily, given, lgiven, middle, lmiddle, nickname, lnickname, bday, lbday, ctry, lctry, locality, llocality, email, lemail, orgname, lorgname, orgunit, lorgunit) FROM stdin;
\.


--
-- Data for Name: vcard_xupdate; Type: TABLE DATA; Schema: public; Owner: "w-495"
--

COPY vcard_xupdate (username, hash, created_at) FROM stdin;
\.


--
-- Name: last_pkey; Type: CONSTRAINT; Schema: public; Owner: "w-495"; Tablespace:
--

ALTER TABLE ONLY last
    ADD CONSTRAINT last_pkey PRIMARY KEY (username);


--
-- Name: motd_pkey; Type: CONSTRAINT; Schema: public; Owner: "w-495"; Tablespace:
--

ALTER TABLE ONLY motd
    ADD CONSTRAINT motd_pkey PRIMARY KEY (username);


--
-- Name: privacy_default_list_pkey; Type: CONSTRAINT; Schema: public; Owner: "w-495"; Tablespace:
--

ALTER TABLE ONLY privacy_default_list
    ADD CONSTRAINT privacy_default_list_pkey PRIMARY KEY (username);


--
-- Name: privacy_list_id_key; Type: CONSTRAINT; Schema: public; Owner: "w-495"; Tablespace:
--

ALTER TABLE ONLY privacy_list
    ADD CONSTRAINT privacy_list_id_key UNIQUE (id);


--
-- Name: pubsub_node_nodeid_key; Type: CONSTRAINT; Schema: public; Owner: "w-495"; Tablespace:
--

ALTER TABLE ONLY pubsub_node
    ADD CONSTRAINT pubsub_node_nodeid_key UNIQUE (nodeid);


--
-- Name: pubsub_state_stateid_key; Type: CONSTRAINT; Schema: public; Owner: "w-495"; Tablespace:
--

ALTER TABLE ONLY pubsub_state
    ADD CONSTRAINT pubsub_state_stateid_key UNIQUE (stateid);


--
-- Name: roster_version_pkey; Type: CONSTRAINT; Schema: public; Owner: "w-495"; Tablespace:
--

ALTER TABLE ONLY roster_version
    ADD CONSTRAINT roster_version_pkey PRIMARY KEY (username);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: "w-495"; Tablespace:
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (username);


--
-- Name: vcard_pkey; Type: CONSTRAINT; Schema: public; Owner: "w-495"; Tablespace:
--

ALTER TABLE ONLY vcard
    ADD CONSTRAINT vcard_pkey PRIMARY KEY (username);


--
-- Name: vcard_search_pkey; Type: CONSTRAINT; Schema: public; Owner: "w-495"; Tablespace:
--

ALTER TABLE ONLY vcard_search
    ADD CONSTRAINT vcard_search_pkey PRIMARY KEY (lusername);


--
-- Name: vcard_xupdate_pkey; Type: CONSTRAINT; Schema: public; Owner: "w-495"; Tablespace:
--

ALTER TABLE ONLY vcard_xupdate
    ADD CONSTRAINT vcard_xupdate_pkey PRIMARY KEY (username);


--
-- Name: i_despool; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_despool ON spool USING btree (username);


--
-- Name: i_irc_custom_jid_host; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE UNIQUE INDEX i_irc_custom_jid_host ON irc_custom USING btree (jid, host);


--
-- Name: i_muc_registered_jid_host; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE UNIQUE INDEX i_muc_registered_jid_host ON muc_registered USING btree (jid, host);


--
-- Name: i_muc_registered_nick; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_muc_registered_nick ON muc_registered USING btree (nick);


--
-- Name: i_muc_room_name_host; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE UNIQUE INDEX i_muc_room_name_host ON muc_room USING btree (name, host);


--
-- Name: i_privacy_list_username; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_privacy_list_username ON privacy_list USING btree (username);


--
-- Name: i_privacy_list_username_name; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE UNIQUE INDEX i_privacy_list_username_name ON privacy_list USING btree (username, name);


--
-- Name: i_private_storage_username; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_private_storage_username ON private_storage USING btree (username);


--
-- Name: i_private_storage_username_namespace; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE UNIQUE INDEX i_private_storage_username_namespace ON private_storage USING btree (username, namespace);


--
-- Name: i_pubsub_item_itemid; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_pubsub_item_itemid ON pubsub_item USING btree (itemid);


--
-- Name: i_pubsub_item_tuple; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item USING btree (nodeid, itemid);


--
-- Name: i_pubsub_node_option_nodeid; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option USING btree (nodeid);


--
-- Name: i_pubsub_node_owner_nodeid; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner USING btree (nodeid);


--
-- Name: i_pubsub_node_parent; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_pubsub_node_parent ON pubsub_node USING btree (parent);


--
-- Name: i_pubsub_node_tuple; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node USING btree (host, node);


--
-- Name: i_pubsub_state_jid; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_pubsub_state_jid ON pubsub_state USING btree (jid);


--
-- Name: i_pubsub_state_tuple; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state USING btree (nodeid, jid);


--
-- Name: i_pubsub_subscription_opt; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt USING btree (subid, opt_name);


--
-- Name: i_rosteru_jid; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_rosteru_jid ON rosterusers USING btree (jid);


--
-- Name: i_rosteru_user_jid; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE UNIQUE INDEX i_rosteru_user_jid ON rosterusers USING btree (username, jid);


--
-- Name: i_rosteru_username; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_rosteru_username ON rosterusers USING btree (username);


--
-- Name: i_sr_user_grp; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_sr_user_grp ON sr_user USING btree (grp);


--
-- Name: i_sr_user_jid; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_sr_user_jid ON sr_user USING btree (jid);


--
-- Name: i_sr_user_jid_grp; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE UNIQUE INDEX i_sr_user_jid_grp ON sr_user USING btree (jid, grp);


--
-- Name: i_vcard_search_lbday; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_vcard_search_lbday ON vcard_search USING btree (lbday);


--
-- Name: i_vcard_search_lctry; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_vcard_search_lctry ON vcard_search USING btree (lctry);


--
-- Name: i_vcard_search_lemail; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_vcard_search_lemail ON vcard_search USING btree (lemail);


--
-- Name: i_vcard_search_lfamily; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_vcard_search_lfamily ON vcard_search USING btree (lfamily);


--
-- Name: i_vcard_search_lfn; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_vcard_search_lfn ON vcard_search USING btree (lfn);


--
-- Name: i_vcard_search_lgiven; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_vcard_search_lgiven ON vcard_search USING btree (lgiven);


--
-- Name: i_vcard_search_llocality; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_vcard_search_llocality ON vcard_search USING btree (llocality);


--
-- Name: i_vcard_search_lmiddle; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_vcard_search_lmiddle ON vcard_search USING btree (lmiddle);


--
-- Name: i_vcard_search_lnickname; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_vcard_search_lnickname ON vcard_search USING btree (lnickname);


--
-- Name: i_vcard_search_lorgname; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_vcard_search_lorgname ON vcard_search USING btree (lorgname);


--
-- Name: i_vcard_search_lorgunit; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX i_vcard_search_lorgunit ON vcard_search USING btree (lorgunit);


--
-- Name: pk_rosterg_user_jid; Type: INDEX; Schema: public; Owner: "w-495"; Tablespace:
--

CREATE INDEX pk_rosterg_user_jid ON rostergroups USING btree (username, jid);


--
-- Name: privacy_list_data_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: "w-495"
--

ALTER TABLE ONLY privacy_list_data
    ADD CONSTRAINT privacy_list_data_id_fkey FOREIGN KEY (id) REFERENCES privacy_list(id) ON DELETE CASCADE;


--
-- Name: pubsub_item_nodeid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: "w-495"
--

ALTER TABLE ONLY pubsub_item
    ADD CONSTRAINT pubsub_item_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES pubsub_node(nodeid) ON DELETE CASCADE;


--
-- Name: pubsub_node_option_nodeid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: "w-495"
--

ALTER TABLE ONLY pubsub_node_option
    ADD CONSTRAINT pubsub_node_option_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES pubsub_node(nodeid) ON DELETE CASCADE;


--
-- Name: pubsub_node_owner_nodeid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: "w-495"
--

ALTER TABLE ONLY pubsub_node_owner
    ADD CONSTRAINT pubsub_node_owner_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES pubsub_node(nodeid) ON DELETE CASCADE;


--
-- Name: pubsub_state_nodeid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: "w-495"
--

ALTER TABLE ONLY pubsub_state
    ADD CONSTRAINT pubsub_state_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES pubsub_node(nodeid) ON DELETE CASCADE;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

