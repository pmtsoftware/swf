{-# LANGUAGE DeriveGeneric #-}

module Config (AppConfig(..), loadAppConfig) where

import Relude

import System.Envy
import  Configuration.Dotenv

data AppConfig = AppConfig
    { appHost    :: Text   -- APP_HOST
    , appPort    :: Int    -- APP_PORT
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
        , pghost = ""
        , pgport = 5432
        , pgdatabase = "swf"
        , pguser = "swf"
        }

instance FromEnv AppConfig

loadAppConfig :: IO AppConfig
loadAppConfig = do
    loadFile defaultConfig -- loading env variables from .env* file
    fromRight defConfig <$> decodeEnv @AppConfig
