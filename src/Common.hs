{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common
    ( AppEnv(..)
    , App(..)
    , module Relude
    , module Config
    , module Database.PostgreSQL.Simple
    , module Database.PostgreSQL.Simple.FromField
    , module Database.PostgreSQL.Simple.ToField
    , module Database.PostgreSQL.Simple.SqlQQ
    , module Data.Pool
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

data AppEnv = AppEnv
    { cfg :: AppConfig
    , connPool :: Pool Connection
    , sessionKey :: Key
    }

newtype App a = App { runApp :: ReaderT AppEnv (LoggingT IO) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppEnv, MonadUnliftIO, MonadLogger)
