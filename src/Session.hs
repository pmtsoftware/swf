{-# OPTIONS_GHC -Wno-orphans #-}

module Session
( auth
, ensureSession
) where

import Common
import Types
import Homepage (layoutM)

import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT, ActionT)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text
import Data.Password.Argon2 (Password, mkPassword, PasswordHash (..), Argon2, checkPassword, PasswordCheck (..))
import qualified Data.Serialize as Bin
import Data.Serialize.Text ()
import Control.Monad.Logger (logErrorN)
import qualified Web.ClientSession as Sess
import qualified Web.Scotty.Cookie as Cookie
import Data.Time.Clock (UTCTime(..), DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime, getCurrentTime)
import Data.Time.Calendar (Day (..), toModifiedJulianDay)

instance Bin.Serialize Day where
  put = Bin.put . toModifiedJulianDay
  get = ModifiedJulianDay <$> Bin.get
instance Bin.Serialize DiffTime where
  put = Bin.put . diffTimeToPicoseconds
  get = picosecondsToDiffTime <$> Bin.get
instance Bin.Serialize UTCTime where
  put UTCTime {..} = Bin.put utctDay >> Bin.put utctDayTime
  get = UTCTime <$> Bin.get <*> Bin.get

data SessionData = MkSessionData
    { sessionEmail :: !Text
    , sessionUserId :: !UserId
    , sessionTimestamp :: !UTCTime -- TODO: change name to expiration and validate
    }
    deriving (Show, Eq, Generic)
instance Bin.Serialize SessionData

data Form = Form
    { formEmail :: !Text
    , formPassword :: !Text
    }

def :: Form
def = Form
    { formEmail = ""
    , formPassword = ""
    }

auth :: ScottyT App ()
auth = do
    Scotty.get "/login" $ loginForm def
    Scotty.post "/login" login
    Scotty.get "/login-successed" $ do
        layout <- layoutM
        Scotty.html . renderHtml $ layout (h1 "Zalogowano")
    Scotty.get "/login-failed" $ do
        layout <- layoutM
        Scotty.html . renderHtml $ layout $ do
            h1 "Logowanie nieudane"
            a ! href "/login" $ "Spróbuj ponownie"
    Scotty.get "/account-locked" $ do
        layout <- layoutM
        Scotty.html . renderHtml $ layout $ h1 "Account locked due to invalid login attempts."

loginForm :: Form -> ActionT App ()
loginForm Form{..} = do
        layout <- layoutM
        Scotty.html . renderHtml $ layout $ do
            h1 "Login"
            form ! method "POST" $ do
                label $ do
                    "Email"
                    input ! required "required" ! name "email" ! type_ "email" ! value (toValue formEmail)
                label $ do
                    "Password"
                    input ! required "required" ! name "password" ! type_ "password" ! value (toValue formPassword)
                button ! type_ "submit" $ "Login"

maxLoginAttempts :: Int
maxLoginAttempts = 5

login :: ActionT App ()
login = do
    Form{..} <- Form
        <$> Scotty.formParam "email"
        <*> Scotty.formParam "password"
    AppEnv{..} <- lift ask
    result <- liftIO $ withResource connPool $ \conn -> query conn stmt (Only formEmail)
    check result (mkPassword formPassword) formEmail
    Scotty.redirect "/login-failed"
    where
        check :: [(UserId, PasswordHash Argon2, Int)] -> Password -> Text -> ActionT App ()
        check [(uid, pHash, fla)] userPass userEmail
            | fla > maxLoginAttempts = do
                Scotty.raw mempty
                Scotty.redirect "/account-locked"
            | otherwise = case checkPassword userPass pHash of
                PasswordCheckSuccess -> do
                    k <- lift $ asks sessionKey
                    ct <- liftIO getCurrentTime
                    let session = MkSessionData userEmail uid ct
                        sessionBS = Bin.encode session
                    encrypted <- liftIO $ Sess.encryptIO k  sessionBS
                    Cookie.setSimpleCookie "swf-session" $ decodeUtf8 encrypted
                    Scotty.redirect "/login-successed"
                PasswordCheckFail -> do
                    lift . logErrorN $ "Invalid password"
                    [Only fla'] <- queryDb @(Only Text) @(Only Int) updateFLA $ Only userEmail
                    when (fla' > maxLoginAttempts) $ do
                        _ <- executeDb setLockedAt $ Only userEmail
                        Scotty.raw mempty
                        Scotty.redirect "/account-locked"
                    Scotty.redirect "/login-failed"
        check _ _ _ = do
            lift . logErrorN $ "User not found"
            Scotty.redirect "/login-failed"

        stmt = [sql|
            SELECT id, password, failed_login_attempts FROM users WHERE email = ?
        |]
        updateFLA = [sql|
            UPDATE users SET failed_login_attempts = failed_login_attempts + 1 WHERE email = ? RETURNING failed_login_attempts;
        |]
        setLockedAt = [sql|
            UPDATE users SET locket_at = transaction_timestamp() WHERE email = ?;
        |]

incFailedLoginAttempts :: Connection -> Email -> IO Int64
incFailedLoginAttempts conn email = execute conn undefined $ Only email
    where
        stmt = [sql|
            UPDATE users SET failed_login_attempts = failed_login_attempts + 1 WHERE email = ? RETURNING failed_login_attempts;
        |]

ensureSession :: ActionT App ()
ensureSession = do
    k <- lift $ asks sessionKey
    encrypted <- Cookie.getCookie "swf-session"
    let decrypted = encrypted >>= Sess.decrypt k . encodeUtf8
        maybeSessionData = decrypted >>= decodeSession
    whenNothing_ maybeSessionData $ Scotty.redirect "/login"
    where
        decodeSession :: ByteString -> Maybe SessionData
        decodeSession = rightToMaybe . Bin.decode @SessionData

