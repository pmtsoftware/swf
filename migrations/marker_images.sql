CREATE TABLE marker_images (
    id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    request_id BIGINT REFERENCES marker_requests (id),
    block_id TEXT NOT NULL,
    image TEXT NOT NULL
);
