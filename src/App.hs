module App
    ( start
    , startWithConfig
    ) where

import Relude

import Config
import Homepage

import qualified Web.Scotty as Scotty

start :: IO ()
start = loadAppConfig >>= startWithConfig

startWithConfig :: AppConfig -> IO ()
startWithConfig AppConfig{..} = do
    Scotty.scotty appPort $
        Scotty.get "/" $ do
            Scotty.html renderHomepage
