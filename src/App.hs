module App
    ( start
    , startWithConfig
    ) where

import Common

import Users
import Homepage
import Db
import qualified Webauthn.Service as Webauthn

import qualified Web.Scotty.Trans as Scotty

import Web.Scotty.Trans (ScottyT)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import qualified Network.Wai.Handler.Warp as Warp
import Control.Monad.Logger (runStdoutLoggingT)
import Web.ClientSession (getDefaultKey)
import Session (auth, ensureSession)
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString.Base16 as Base16
import Webauthn.PendingCeremonies (newPendingCeremonies, defaultPendingCeremoniesConfig)
import Webauthn.MetadataFetch (emptyRegistry)
import Crypto.WebAuthn (RpIdHash(..))
import qualified Crypto.Hash as Hash
import Network.Wai (Request)

runIO :: AppEnv -> App a -> IO a
runIO env = runStdoutLoggingT . usingReaderT env . runApp

start :: Bool -> IO ()
start dev = loadAppConfig >>= startWithConfig dev nop

exceptionHandler :: Maybe Request -> SomeException -> IO ()
exceptionHandler _ err = do
    putStrLn "EXCEPTION!"
    print err

startWithConfig :: Bool -> IO () -> AppConfig -> IO ()
startWithConfig dev beforeMainLoop cfg@AppConfig{..} = do
    let poolCfg = defaultPoolConfig
                    (connectPostgreSQL "")
                    close
                    60
                    10
    pool <- newPool $ setNumStripes (Just 1) poolCfg
    _ <- withResource pool migrateDb
    key <- getDefaultKey
    cssChecksum <- buildCssChecksum
    pendingCeremonies <- newPendingCeremonies defaultPendingCeremoniesConfig
    registry <- newTVarIO emptyRegistry
    -- The RP ID is the effective domain (e.g. "localhost"); its SHA-256 hash is
    -- what the authenticator signs over. The origin is the full scheme+host+port
    -- the browser reports. Both come from config so non-localhost deployments
    -- (and https) work without code changes.
    let rpIdHash = RpIdHash $ Hash.hash $ encodeUtf8 @Text @ByteString appHost
        origin = fromString $ toString appOrigin
    let env = AppEnv cfg pool key cssChecksum pendingCeremonies registry rpIdHash origin dev
        warpSettings = Warp.setPort appPort
            . Warp.setBeforeMainLoop beforeMainLoop
            . Warp.setOnException exceptionHandler
            $ Warp.defaultSettings
        webOpts = Scotty.defaultOptions { Scotty.settings = warpSettings }
    Scotty.scottyOptsT webOpts (runIO env) application

nop :: IO ()
nop = return ()

application :: ScottyT App ()
application = do
        Scotty.matchAny staticRoute sApp
        Scotty.get "/" $ do
            ensureSession
            AppEnv{..} <- lift ask
            logInfo "GET home page"
            Scotty.html $ renderHomepage dev cssChecksum
        users
        auth
        Webauthn.service
    where
        staticRoute = Scotty.regex "^/static/(.*)"
        sApp = Scotty.nested $ staticApp $ defaultWebAppSettings "."

buildCssChecksum :: IO ByteString
buildCssChecksum = Base16.encode . hash <$> readFileBS "./static/swf.css"
