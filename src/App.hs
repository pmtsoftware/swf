module App
    ( start
    , startWithConfig
    ) where

import Common

import Users
import Homepage
import Db

import qualified Web.Scotty.Trans as Scotty

import Web.Scotty.Trans (ScottyT)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Control.Monad.Logger (runStdoutLoggingT, logInfoN)

runIO :: AppEnv -> App a -> IO a
runIO env = runStdoutLoggingT . usingReaderT env . runApp

start :: IO ()
start = loadAppConfig >>= startWithConfig

startWithConfig :: AppConfig -> IO ()
startWithConfig cfg@AppConfig{..} = do
    let poolCfg = defaultPoolConfig
                    (connectPostgreSQL "")
                    close
                    60
                    10
    pool <- newPool $ setNumStripes (Just 1) poolCfg
    _ <- withResource pool migrateDb
    let env = AppEnv cfg pool
    Scotty.scottyT appPort (runIO env) application

application :: ScottyT App ()
application = do
        Scotty.matchAny staticRoute sApp
        Scotty.get "/" $ do
            lift $ logInfoN "GET home page"
            Scotty.html renderHomepage
        users
    where
        staticRoute = Scotty.regex "^/static/(.*)"
        sApp = Scotty.nested $ staticApp $ defaultWebAppSettings "."
