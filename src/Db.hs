module Db
    ( migrateDb
    ) where

import Relude

import Hasql.Migration
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Hasql.Pool

migrateDb :: Pool -> IO (Either Text ())
migrateDb pool = do
    commands <- scripts
    let session = transaction ReadCommitted Write $ runMigrations commands
    result <- use pool session
    case result of
        Left usageErr -> pure . Left . toText . displayException $ usageErr
        Right (Just err) -> pure . Left . toText . show @String $ err
        Right Nothing -> pure . Right $ ()

runMigrations :: [MigrationCommand] -> Transaction (Maybe MigrationError)
runMigrations [] = pure Nothing
runMigrations (c:cs) = do
    result <- runMigration c
    case result of
        Just err -> Just err <$ condemn
        Nothing -> runMigrations cs

scripts :: IO [MigrationCommand]
scripts = sequenceA
    [ pure MigrationInitialization
    , loadMigrationFromFile "users.sql" "./migrations/users.sql"
    , loadMigrationFromFile "email_unique.sql" "./migrations/email_unique.sql"
    , loadMigrationFromFile "webauthn.sql" "./migrations/webauthn.sql"
    , loadMigrationFromFile "credential_user_handle.sql" "./migrations/credential_user_handle.sql"
    ]
