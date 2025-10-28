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
    pendingCeremonies <- newPendingCeremonies defaultPendingCeremoniesConfig
    registry <- newTVarIO emptyRegistry
    let rpIdHash = RpIdHash $ Hash.hash $ encodeUtf8 @Text @ByteString "localhost"
    let env = AppEnv cfg pool key cssChecksum pendingCeremonies registry rpIdHash
        warpSettings = Warp.setPort appPort
            . Warp.setBeforeMainLoop beforeMainLoop
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
            checksum <- lift $ asks cssChecksum
            logInfo "GET home page"
            Scotty.html $ renderHomepage checksum
        users
        auth
    where
        staticRoute = Scotty.regex "^/static/(.*)"
        sApp = Scotty.nested $ staticApp $ defaultWebAppSettings "."

buildCssChecksum :: IO ByteString
buildCssChecksum = Base16.encode . hash <$> readFileBS "./static/swf.css"
