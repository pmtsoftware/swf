{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common
    ( AppEnv(..)
    , App(..)
    , Handler
    , module Relude
    , module Config
    , module Database.PostgreSQL.Simple
    , module Database.PostgreSQL.Simple.FromField
    , module Database.PostgreSQL.Simple.ToField
    , module Database.PostgreSQL.Simple.SqlQQ
    , module Data.Pool
    , module Fmt
    , queryDb
    , executeDb
    ) where

import Relude hiding (div, head, id, span, map)

import Config

import Database.PostgreSQL.Simple hiding (fold)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.ToField (ToField (toField))
import Data.Pool
import UnliftIO (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Web.ClientSession (Key)
import Web.Scotty.Trans (ActionT)
import Fmt ((+|), (|+))

data AppEnv = AppEnv
    { cfg :: AppConfig
    , connPool :: Pool Connection
    , sessionKey :: Key
    , cssChecksum :: ByteString
    }

newtype App a = App { runApp :: ReaderT AppEnv (LoggingT IO) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppEnv, MonadUnliftIO, MonadLogger)

type Handler a = ActionT App a

queryDb :: (ToRow q, FromRow r) => Query -> q -> Handler [r]
queryDb stmt args = do
    connPool <- lift $ asks connPool
    liftIO $ withResource connPool $ \conn -> query conn stmt args

executeDb :: (ToRow q) => Query -> q -> Handler Int64
executeDb stmt args = do
    connPool <- lift $ asks connPool
    liftIO $ withResource connPool $ \conn -> execute conn stmt args
