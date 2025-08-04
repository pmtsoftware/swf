CREATE TABLE marker_requests (
    id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    request_id TEXT NOT NULL,
    request_check_url TEXT NOT NULL,
    status TEXT NOT NULL,
    success BOOLEAN,
    checkpoint_id TEXT,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL
);

CREATE TABLE marker_blocks (
    id BIGINT GENERATED ALWAYS AS IDENTITY,
    request_id BIGINT REFERENCES marker_requests (id),
    blockId TEXT NOT NULL,
    html TEXT NOT NULL,
    block_type TEXT NOT NULL
);
