module App
    ( start
    , startWithConfig
    ) where

import Common

import Users
import Homepage
import Db

import qualified Marker as Marker

import qualified Web.Scotty.Trans as Scotty

import Web.Scotty.Trans (ScottyT)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad.Logger (runStdoutLoggingT, logInfoN)
import Web.ClientSession (getDefaultKey)
import Session (auth, ensureSession)
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString.Base16 as Base16
import Marker (pollMarkerJobResult)
import Control.Concurrent (forkIO)
import qualified Network.Wai.Middleware.BearerTokenAuth as Bearer
import qualified Network.Wai.Middleware.Cors as Cors
import Network.Wai.Middleware.HealthCheckEndpoint (healthCheck)
import Network.Wai.Middleware.RequestLogger (logStdout)

apiCors :: Cors.CorsResourcePolicy
apiCors = Cors.simpleCorsResourcePolicy
    { Cors.corsMethods = Cors.simpleMethods <> ["OPTIONS", "DELETE"]
    , Cors.corsRequestHeaders = Cors.simpleHeaders <> ["Content-Type", "Authorization"]
    }

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
    markerMVar <- newEmptyMVar
    let env = AppEnv cfg pool key cssChecksum markerMVar
        warpSettings = Warp.setPort appPort
            . Warp.setBeforeMainLoop beforeMainLoop
            $ Warp.defaultSettings
        webOpts = Scotty.defaultOptions { Scotty.settings = warpSettings }
    _ <- forkIO $ pollMarkerJobResult env
    Scotty.scottyOptsT webOpts (runIO env) $ application cfg

nop :: IO ()
nop = return ()

application :: AppConfig -> ScottyT App ()
application AppConfig{..} = do
    -- Scotty.middleware logStdout
    Scotty.middleware healthCheck
    Scotty.middleware $ Cors.cors (const (Just apiCors))
    Scotty.middleware  $ Bearer.tokenListAuth [encodeUtf8 secret]
    Marker.service

buildCssChecksum :: IO ByteString
buildCssChecksum = Base16.encode . hash <$> readFileBS "./static/swf.css"
