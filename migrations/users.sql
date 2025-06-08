CREATE TABLE users (
    id bigint GENERATED ALWAYS AS IDENTITY,
    email TEXT NOT NULL,
    password TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL
)
