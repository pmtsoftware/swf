module App
    ( start
    , startWithConfig
    ) where

import Common

import Db

import qualified Web.Scotty.Trans as Scotty

import Web.Scotty.Trans (ScottyT)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad.Logger (runStdoutLoggingT, logInfoN)
import Web.ClientSession (getDefaultKey)
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString.Base16 as Base16
import qualified Network.Wai.Middleware.BearerTokenAuth as Bearer
import Data.Aeson

runIO :: AppEnv -> App a -> IO a
runIO env = runStdoutLoggingT . usingReaderT env . runApp

start :: IO ()
start = loadAppConfig >>= startWithConfig nop

startWithConfig :: IO () -> AppConfig -> IO ()
startWithConfig beforeMainLoop cfg@AppConfig{..} = do
    let poolCfg = defaultPoolConfig
                    (connectPostgreSQL "")
                    close
                    60
                    10
    pool <- newPool $ setNumStripes (Just 1) poolCfg
    _ <- withResource pool migrateDb
    key <- getDefaultKey
    cssChecksum <- buildCssChecksum
    let env = AppEnv cfg pool key cssChecksum
        warpSettings = Warp.setPort appPort
            . Warp.setBeforeMainLoop beforeMainLoop
            $ Warp.defaultSettings
        webOpts = Scotty.defaultOptions { Scotty.settings = warpSettings }
    Scotty.scottyOptsT webOpts (runIO env) $ application cfg

nop :: IO ()
nop = return ()

data Ok = Ok { ok :: Bool }
    deriving (Show, Generic)
instance ToJSON Ok where
    toEncoding = genericToEncoding defaultOptions

application :: AppConfig -> ScottyT App ()
application AppConfig{..} = do
        Scotty.middleware  $ Bearer.tokenListAuth [encodeUtf8 secret]
        Scotty.matchAny staticRoute sApp
        Scotty.get "/" $ do
            Scotty.json $ Ok True
    where
        staticRoute = Scotty.regex "^/static/(.*)"
        sApp = Scotty.nested $ staticApp $ defaultWebAppSettings "."

buildCssChecksum :: IO ByteString
buildCssChecksum = Base16.encode . hash <$> readFileBS "./static/swf.css"
