module Auth.Session where

import Common

import Auth.Types

import qualified Web.ClientSession as Sess
import qualified Web.Scotty.Cookie as Cookie
import qualified Data.Serialize as Bin
import Web.Scotty.Trans (ActionT)
import qualified Web.Scotty.Trans as Scotty
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime, secondsToNominalDiffTime)

cookieName :: ByteString
cookieName = "swf-session"

sessionLifetime :: NominalDiffTime
sessionLifetime = secondsToNominalDiffTime $ 60 * 60 * 12

writeSession :: Sess.Key -> SessionData -> ActionT App ()
writeSession k sd = do
    encrypted <- liftIO $ Sess.encryptIO k (Bin.encode sd)
    now <- liftIO getCurrentTime
    dev <- lift $ asks dev
    Cookie.setCookie Cookie.defaultSetCookie
        { Cookie.setCookieName = cookieName
        , Cookie.setCookieValue = encrypted
        , Cookie.setCookiePath = Just "/"
        , Cookie.setCookieHttpOnly = True
        , Cookie.setCookieSecure = not dev
        , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
        , Cookie.setCookieMaxAge = Just . realToFrac $ diffUTCTime (sessionExpiry sd) now
        }

authOr :: Handler () -> (SessionData -> Handler ()) -> Handler ()
authOr unlogged logged = do
    k <- lift $ asks sessionKey
    now <- liftIO getCurrentTime
    let decode = rightToMaybe . Bin.decode @SessionData

    mSession <- runMaybeT $ do
        raw <- MaybeT $ Cookie.getCookie $ decodeUtf8 cookieName
        sd <- MaybeT . pure $ Sess.decrypt k (encodeUtf8 raw) >>= decode
        guard (now < sessionExpiry sd)
        pure sd

    maybe unlogged logged mSession

auth :: (SessionData -> Handler ()) -> Handler ()
auth = authOr (Scotty.redirect "/login")

