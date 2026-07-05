{-# LANGUAGE DeriveGeneric #-}

module Config
    ( AppConfig(..)
    , loadAppConfig
    , loadTestConfig
    ) where

import Relude

import System.Envy
import LoadEnv (loadEnv, loadEnvFrom)

data AppConfig = AppConfig
    { appHost    :: Text   -- APP_HOST
    , appPort    :: Int    -- APP_PORT
    , appOrigin  :: Text   -- APP_ORIGIN
    , pghost     :: Text   -- PGHOST
    , pgport     :: Int    -- PGPORT
    , pgdatabase :: Text   -- PGDATABASE
    , pguser     :: Text   -- PGUSER
    }
    deriving (Generic, Show)

instance DefConfig AppConfig where
    defConfig = AppConfig
        { appHost = "localhost"
        , appPort = 3000
        , appOrigin = "http://localhost:3000"
        , pghost = ""
        , pgport = 5432
        , pgdatabase = "swf"
        , pguser = "swf"
        }

instance FromEnv AppConfig

loadAppConfig :: IO AppConfig
loadAppConfig = do
    loadEnv -- loading env variables from .env file
    fromRight defConfig <$> decodeEnv @AppConfig

loadTestConfig :: IO AppConfig
loadTestConfig = do
    loadEnvFrom ".env.test" -- loading env variables from .env.test file
    fromRight defConfig <$> decodeEnv @AppConfig
