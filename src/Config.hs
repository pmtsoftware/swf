{-# LANGUAGE DeriveGeneric #-}

module Config
    ( AppConfig(..)
    , loadAppConfig
    , loadTestConfig
    ) where

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
    , secret     :: Text   -- SECRET
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
        , secret = error "SECRET must be provided"
        }

instance FromEnv AppConfig

loadAppConfig :: IO AppConfig
loadAppConfig = loadConfig defaultConfig

loadTestConfig :: IO AppConfig
loadTestConfig = loadConfig $ defaultConfig { configPath = [ ".env.test" ] }

loadConfig :: Config -> IO AppConfig
loadConfig cfg = do
    loadFile cfg -- loading env variables from .env* file
    fromRight defConfig <$> decodeEnv @AppConfig
