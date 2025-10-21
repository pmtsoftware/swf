module Main (main) where

import Relude

import App
import CommandLine
import Users (createUser, Form(..))

import Database.PostgreSQL.Simple hiding (fold)
import Config (loadAppConfig)

main :: IO ()
main = do
    opt <- parseCommand
    case opt of
        App -> start
        AddUser un pwd -> do
            _ <- loadAppConfig
            conn <- connectPostgreSQL ""
            result <- createUser conn $ Form Nothing (encodeUtf8 un) pwd pwd
            case result of
                Left _ -> putStrLn "Error"
                Right _ -> putStrLn " User added"
