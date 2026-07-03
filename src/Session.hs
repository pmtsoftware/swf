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

import Hasql.TH
import Hasql.Statement
import Hasql.Decoders hiding (text)
import Hasql.Encoders hiding (name)
import Hasql.Session --(statement)
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as Tr
import Data.Profunctor

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

data Error
    = EmailNotFound
    | AccountLocked
    | InvalidPassword

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
    Scotty.get "/login" $ loginForm def Nothing
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

loginForm :: Form -> Maybe Error -> ActionT App ()
loginForm Form{..} err = do
        layout <- layoutM
        Scotty.html . renderHtml $ layout $ do
            h1 "Login"
            form ! method "POST" $ do
                label $ do
                    "Email"
                    input ! required "required" ! name "email" ! type_ "email" ! value (toValue formEmail)
                    whenJust err renderEmailErr
                label $ do
                    "Password"
                    input ! required "required" ! name "password" ! type_ "password" ! value (toValue formPassword)
                    whenJust err renderPassErr
                button ! type_ "submit" $ "Login"
    where
        renderEmailErr :: Error -> Html
        renderEmailErr EmailNotFound = ul $ li "Email not found"
        renderEmailErr AccountLocked = ul $ li "Account locked due to failed login attempts"
        renderEmailErr _ = mempty

        renderPassErr :: Error -> Html
        renderPassErr InvalidPassword = ul $ li "Invalid password"
        renderPassErr _ = mempty

maxLoginAttempts :: Int32
maxLoginAttempts = 5

login :: ActionT App ()
login = do
    Form{..} <- Form
        <$> Scotty.formParam "email"
        <*> Scotty.formParam "password"
    pool <- lift $ asks connPool
    result <- liftIO $ use pool $ statement formEmail selectPassword --queryDb stmt (Only formEmail)
    case result of
        Left err -> error "Failed"
        Right Nothing -> loginForm (Form formEmail "") $ Just EmailNotFound
        Right (Just row) -> check row (mkPassword formPassword) formEmail
    where
        check :: (UserId, PasswordHash Argon2, Int32) -> Password -> Text -> ActionT App ()
        check (uid, pHash, fla) userPass userEmail
            | fla > maxLoginAttempts = loginForm (Form userEmail "") $ Just AccountLocked
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
                    -- [Only fla'] <- queryDb @(Only Text) @(Only Int) updateFLA $ Only userEmail
                    -- when (fla' > maxLoginAttempts) $ do
                        -- _ <- executeDb setLockedAt $ Only userEmail
                        -- Scotty.redirect "/account-locked"
                    loginForm (Form userEmail "") $ Just InvalidPassword

id' :: a -> a
id' x = x

selectPassword :: Statement Text (Maybe (UserId, PasswordHash Argon2, Int32))
selectPassword = dimap id' (fmap toResult) [maybeStatement|
        SELECT id :: int8, password :: text, failed_login_attempts :: int4 FROM users WHERE email = $1 :: text
    |]
    where
        toResult :: (Int64, Text, Int32) -> (UserId, PasswordHash Argon2, Int32)
        toResult (x, y, z) = (UserId x, PasswordHash y, z)

updateLoginAttemps :: UserId -> Transaction ()
updateLoginAttemps (UserId uid) = do
    fla <- Tr.statement uid [singletonStatement|
        UPDATE users SET failed_login_attempts = failed_login_attempts + 1 WHERE id = $1 :: int8 RETURNING failed_login_attempts :: int4
    |]
    when (fla > maxLoginAttempts) $ do
        -- lock account
        Tr.statement uid [resultlessStatement|
            UPDATE users SET locked_at = transaction_timestamp() WHERE id = $1 :: int8
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

