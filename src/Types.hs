{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Types
    ( UserId(..)
    , Email(..)
    ) where

import Common

import TextShow hiding (toString, toText)
import qualified Data.Serialize as Bin
import Data.Password.Argon2 (PasswordHash (..))

newtype UserId = UserId { unUserId :: Int64 }
    deriving (Show, Eq, Generic)
deriving newtype instance FromField UserId
deriving newtype instance ToField UserId
deriving newtype instance TextShow UserId
deriving newtype instance Bin.Serialize UserId

newtype Email = Email { unEmail :: Text }
    deriving (Show, Eq, Generic)
deriving newtype instance FromField Email
deriving newtype instance ToField Email

deriving instance Generic (PasswordHash a)
deriving newtype instance FromField (PasswordHash a)
deriving newtype instance ToField (PasswordHash a)
