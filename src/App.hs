{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App
    ( start
    , startWithConfig
    ) where

import Relude

import Config
import Homepage
import Db

import qualified Web.Scotty.Trans as Scotty

import Database.PostgreSQL.Simple
import Data.Pool
import Web.Scotty.Trans (ScottyT)
import UnliftIO (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT, logInfoN, MonadLogger)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)

data AppEnv = AppEnv
    { cfg :: AppConfig
    , connPool :: Pool Connection
    }

newtype App a = App { runApp :: ReaderT AppEnv (LoggingT IO) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppEnv, MonadUnliftIO, MonadLogger)

runIO :: AppEnv -> App a -> IO a
runIO env = runStdoutLoggingT . usingReaderT env . runApp

start :: IO ()
start = loadAppConfig >>= startWithConfig

startWithConfig :: AppConfig -> IO ()
startWithConfig cfg@AppConfig{..} = do
    let poolCfg = defaultPoolConfig
                    (connectPostgreSQL "")
                    close
                    60
                    10
    pool <- newPool $ setNumStripes (Just 1) poolCfg
    _ <- withResource pool migrateDb
    let env = AppEnv cfg pool
    Scotty.scottyT appPort (runIO env) application

application :: ScottyT App ()
application = do
        Scotty.matchAny staticRoute sApp
        Scotty.get "/" $ do
            lift $ logInfoN "GET home page"
            Scotty.html renderHomepage

    where
        staticRoute = Scotty.regex "^/static/(.*)"
        sApp = Scotty.nested $ staticApp $ defaultWebAppSettings "."
