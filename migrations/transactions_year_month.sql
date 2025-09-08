ALTER TABLE transactions ADD COLUMN year INT;
ALTER TABLE transactions ADD COLUMN month INT;

UPDATE transactions SET year = EXTRACT(YEAR FROM created_at), month = EXTRACT(MONTH FROM created_at);

ALTER TABLE transactions ALTER COLUMN year SET NOT NULL;
ALTER TABLE transactions ALTER COLUMN month SET NOT NULL;

CREATE INDEX transactions_year_index ON transactions (year);
CREATE INDEX transactions_month_index ON transactions (month);
