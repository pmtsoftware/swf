{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App
    ( start
    , startWithConfig
    ) where

import Relude

import Config
import Homepage

import qualified Web.Scotty.Trans as Scotty

import Database.PostgreSQL.Simple
import Data.Pool
import Web.Scotty.Trans (ScottyT)
import UnliftIO (MonadUnliftIO)

data AppEnv = AppEnv
    { envs :: AppConfig
    , connPool :: Pool Connection
    }

newtype App a = App { runApp :: ReaderT AppEnv IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadUnliftIO)

evalApp :: AppEnv -> App a -> IO a
evalApp env = usingReaderT env . runApp

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
    let env = AppEnv cfg pool
    Scotty.scottyT appPort (evalApp env) application

application :: ScottyT App ()
application = do
        Scotty.get "/" $ do
            Scotty.html renderHomepage
