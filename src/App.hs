module App (run) where

import Relude

import Config
import Homepage

import qualified Web.Scotty as Scotty

run :: IO ()
run = do
    AppConfig{..} <- loadAppConfig
    Scotty.scotty appPort $
        Scotty.get "/" $ do
            Scotty.html renderHomepage
