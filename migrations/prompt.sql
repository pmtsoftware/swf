CREATE TABLE prompt_templates (
    id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    prompt TEXT NOT NULL DEFAULT ''
);

INSERT INTO prompt_templates DEFAULT VALUES;

CREATE TABLE prompt_history (
    id BIGINT GENERATED ALWAYS AS IDENTITY,
    request_id BIGINT REFERENCES marker_requests (id),
    prompt TEXT NOT NULL
);
