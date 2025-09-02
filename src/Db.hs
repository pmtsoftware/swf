module Db
    ( migrateDb
    ) where

import Relude

import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple

migrateDb :: Connection -> IO (MigrationResult String)
migrateDb conn = runMigrations conn defaultOptions
    [ MigrationInitialization
    , MigrationFile "users.sql" "./migrations/users.sql"
    , MigrationFile "email_unique.sql" "./migrations/email_unique.sql"
    , MigrationFile "marker.sql" "./migrations/marker.sql"
    , MigrationFile "marker_page_no.sql" "./migrations/marker_page_no.sql"
    , MigrationFile "marker_page_order.sql" "./migrations/marker_page_order.sql"
    , MigrationFile "prompt.sql" "./migrations/prompt.sql"
    ]
