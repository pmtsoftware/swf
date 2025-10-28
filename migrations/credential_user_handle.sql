ALTER TABLE credential_entries ADD user_handle BYTEA REFERENCES users (handle) NOT NULL;

ALTER TABLE credential_entries DROP column user_id;
