ALTER TABLE users ADD CONSTRAINT users_pkey PRIMARY KEY (id);

ALTER TABLE users ADD handle BYTEA CONSTRAINT users_handle_unique UNIQUE;

ALTER TABLE users ADD display_name TEXT DEFAULT '' NOT NULL;

CREATE TABLE credential_entries (
    credential_id BYTEA PRIMARY KEY,
    user_id BIGINT REFERENCES users (id) NOT NULL,
    public_key BYTEA NOT NULL,
    sign_counter INTEGER NOT NULL,
    transports BYTEA NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL
)
