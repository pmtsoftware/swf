module Main (main) where

import Relude

import App
import CommandLine

main :: IO ()
main = do
    opt <- parseCommand
    case opt of
        App -> start
        AddUser _ _ -> putStrLn "Add user"
        Import _ -> putStrLn "Import"
