
CREATE INDEX i_despool ON spool USING btree (username);

CREATE UNIQUE INDEX i_irc_custom_jid_host ON irc_custom USING btree (jid, host);

CREATE UNIQUE INDEX i_muc_registered_jid_host ON muc_registered USING btree (jid, host);

CREATE INDEX i_muc_registered_nick ON muc_registered USING btree (nick);

CREATE UNIQUE INDEX i_muc_room_name_host ON muc_room USING btree (name, host);

CREATE INDEX i_privacy_list_username ON privacy_list USING btree (username);

CREATE UNIQUE INDEX i_privacy_list_username_name ON privacy_list USING btree (username, name);

CREATE INDEX i_private_storage_username ON private_storage USING btree (username);

CREATE UNIQUE INDEX i_private_storage_username_namespace ON private_storage USING btree (username, namespace);

CREATE INDEX i_pubsub_item_itemid ON pubsub_item USING btree (itemid);

CREATE UNIQUE INDEX i_pubsub_item_tuple ON pubsub_item USING btree (nodeid, itemid);

CREATE INDEX i_pubsub_node_option_nodeid ON pubsub_node_option USING btree (nodeid);

CREATE INDEX i_pubsub_node_owner_nodeid ON pubsub_node_owner USING btree (nodeid);

CREATE INDEX i_pubsub_node_parent ON pubsub_node USING btree (parent);

CREATE UNIQUE INDEX i_pubsub_node_tuple ON pubsub_node USING btree (host, node);

CREATE INDEX i_pubsub_state_jid ON pubsub_state USING btree (jid);

CREATE UNIQUE INDEX i_pubsub_state_tuple ON pubsub_state USING btree (nodeid, jid);

CREATE UNIQUE INDEX i_pubsub_subscription_opt ON pubsub_subscription_opt USING btree (subid, opt_name);

CREATE INDEX i_rosteru_jid ON rosterusers USING btree (jid);

CREATE UNIQUE INDEX i_rosteru_user_jid ON rosterusers USING btree (username, jid);

CREATE INDEX i_rosteru_username ON rosterusers USING btree (username);

CREATE INDEX i_sr_user_grp ON sr_user USING btree (grp);

CREATE INDEX i_sr_user_jid ON sr_user USING btree (jid);

CREATE UNIQUE INDEX i_sr_user_jid_grp ON sr_user USING btree (jid, grp);

CREATE INDEX i_vcard_search_lbday ON vcard_search USING btree (lbday);

CREATE INDEX i_vcard_search_lctry ON vcard_search USING btree (lctry);

CREATE INDEX i_vcard_search_lemail ON vcard_search USING btree (lemail);

CREATE INDEX i_vcard_search_lfamily ON vcard_search USING btree (lfamily);


CREATE INDEX i_vcard_search_lfn ON vcard_search USING btree (lfn);


CREATE INDEX i_vcard_search_lgiven ON vcard_search USING btree (lgiven);


CREATE INDEX i_vcard_search_llocality ON vcard_search USING btree (llocality);


CREATE INDEX i_vcard_search_lmiddle ON vcard_search USING btree (lmiddle);


CREATE INDEX i_vcard_search_lnickname ON vcard_search USING btree (lnickname);


CREATE INDEX i_vcard_search_lorgname ON vcard_search USING btree (lorgname);


CREATE INDEX i_vcard_search_lorgunit ON vcard_search USING btree (lorgunit);


CREATE INDEX pk_rosterg_user_jid ON rostergroups USING btree (username, jid);

