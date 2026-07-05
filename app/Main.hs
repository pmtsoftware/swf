module Main (main) where

import Relude

import App
import CommandLine
import Users (createUser, Form(..), validateForm, reportFormValidationError)

import Data.Validation

import qualified Hasql.Session as Hasql
import qualified Hasql.Connection as Hasql
import qualified Hasql.Connection.Setting as ConnSetting
import qualified Hasql.Connection.Setting.Connection as ConnString
import Data.Password.Argon2 (hashPassword)
import Config (loadAppConfig)

main :: IO ()
main = do
    opt <- parseCommand
    case opt of
        WebApp dev -> start dev
        AddUser un pwd -> do
            _ <- loadAppConfig
            -- hasql 1.9: acquire takes connection Settings; empty string means
            -- libpq falls back to the PG* env vars (as before).
            conn <- Hasql.acquire [ConnSetting.connection (ConnString.string "")]
            -- TODO: password as command line arg is not goo idea because it stays in shell history
            -- one option is to use 'pwgen' tool to generate password randomly
            whenRight_ conn $ \conn' -> do
                let form = toEither . validateForm $ Form Nothing (encodeUtf8 un) pwd pwd
                case form of
                    Left errs -> putTextLn "Errors:" >> forM_ (reportFormValidationError <$> errs) putTextLn
                    Right (e, p) -> do
                        pwHash <- hashPassword p
                        result <- Hasql.run (createUser (e, pwHash)) conn'
                        case result of
                            Left _ -> putStrLn "Error"
                            Right _ -> putStrLn " User added"
