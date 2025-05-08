module Main (main) where

import Relude

import Homepage

import qualified Web.Scotty as Scotty

main :: IO ()
main = Scotty.scotty 3000 $
    Scotty.get "/" $ do
        Scotty.html renderHomepage
