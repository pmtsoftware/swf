{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Types
    ( UserId(..)
    , Email(..)
    ) where

import Common

import qualified Data.Serialize as Bin
import Data.Password.Argon2 (PasswordHash (..))

newtype UserId = UserId { unUserId :: Int64 }
    deriving (Show, Eq, Generic)
deriving newtype instance Bin.Serialize UserId

newtype Email = Email { unEmail :: Text }
    deriving (Show, Eq, Generic)

deriving instance Generic (PasswordHash a)
