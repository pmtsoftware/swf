module App (run) where

import Relude

import Homepage

import qualified Web.Scotty as Scotty

run :: IO ()
run = Scotty.scotty 3000 $
    Scotty.get "/" $ do
        Scotty.html renderHomepage
