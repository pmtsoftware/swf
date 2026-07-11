{-# OPTIONS_GHC -Wno-orphans #-}
module Auth.Types where

import Relude

import Types

import qualified Data.Serialize as Bin
import Data.Serialize.Text ()
import Data.Time.Clock (UTCTime(..), DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Time.Calendar (Day (..), toModifiedJulianDay)

instance Bin.Serialize Day where
  put = Bin.put . toModifiedJulianDay
  get = ModifiedJulianDay <$> Bin.get
instance Bin.Serialize DiffTime where
  put = Bin.put . diffTimeToPicoseconds
  get = picosecondsToDiffTime <$> Bin.get
instance Bin.Serialize UTCTime where
  put UTCTime {..} = Bin.put utctDay >> Bin.put utctDayTime
  get = UTCTime <$> Bin.get <*> Bin.get

data SessionData = MkSessionData
    { sessionEmail :: !Text
    , sessionUserId :: !UserId
    , sessionExpiry :: !UTCTime
    }
    deriving (Show, Eq, Generic)
instance Bin.Serialize SessionData
