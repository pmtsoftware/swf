module Session where

import Common

import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT, ActionT)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text
import Data.Password.Argon2 (Password, mkPassword, PasswordHash (..), Argon2, checkPassword, PasswordCheck (..))
import qualified Data.Binary as Bin
import Homepage (layout)
import Control.Monad.Logger (logErrorN)
import Types

data SessionData = MkSessionData
    { sessionEmail :: !Text
    , sessionUserId :: !UserId
    }
    deriving (Show, Eq, Generic)
instance Bin.Binary SessionData

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
    Scotty.get "/login-successed" $ Scotty.html . renderHtml . layout $ h1 "Zalogowano"
    Scotty.get "/login-failed" $ Scotty.html . renderHtml . layout $ do
        h1 "Logowanie nieudane"
        a ! href "/login" $ "Spróbuj ponownie"

loginForm :: Form -> ActionT App ()
loginForm Form{..} = Scotty.html . renderHtml $ markup
    where
        markup = layout $ do
            h1 "Login"
            form ! method "POST" $ do
                label $ do
                    "Email"
                    input ! required "required" ! name "email" ! type_ "email" ! value (toValue formEmail)
                label $ do
                    "Password"
                    input ! required "required" ! name "password" ! type_ "password" ! value (toValue formPassword)
                button ! type_ "submit" $ "Login"

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
        check :: [(UserId, PasswordHash Argon2)] -> Password -> Text -> ActionT App ()
        check [(uid, pHash)] userPass userEmail = case checkPassword userPass pHash of
            PasswordCheckSuccess -> do
                let session = MkSessionData userEmail uid
                    sessionBS = Bin.encode session
                Scotty.redirect "/login-successed"
            PasswordCheckFail -> do
                lift . logErrorN $ "Invalid password"
                Scotty.redirect "/login-failed"
        check _ _ _ = do
            lift . logErrorN $ "User not found"
            Scotty.redirect "/login-failed"
        stmt = [sql|
            SELECT id, password FROM users WHERE email = ?
        |]

