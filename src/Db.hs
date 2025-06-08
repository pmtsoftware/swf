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
    ]
