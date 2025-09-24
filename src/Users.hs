module Users ( users ) where

import Common hiding (pass)
import Types
import Homepage (layoutM)

import qualified Text.Email.Validate as EmailV
import Text.Email.Validate (EmailAddress)
import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT, ActionT)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text
import qualified Data.List.NonEmpty as NE
import Data.Password.Argon2 (Password, mkPassword, hashPassword)
import Data.Password.Validate
import qualified Data.Text as T
import Database.PostgreSQL.Simple.Time (ZonedTimestamp)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Validation
import Relude.Extra.Newtype (un)
import Network.HTTP.Types (badRequest400)
import Database.PostgreSQL.Simple.Errors (catchViolation, ConstraintViolation (UniqueViolation))
import Control.Exception (throwIO)

newtype PlainPassword = PlainPassword Text
    deriving (Show, Eq)

newtype PlainPassword2 = PlainPassword2 Text
    deriving (Show, Eq)

data User = User
    { userId :: UserId
    , email :: !Email
    , lockedAt :: Maybe ZonedTimestamp
    , failedLoginAttempts :: !Int
    }
    deriving (Show, Generic)
instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field

data Form = Form
    { formUserId :: Maybe UserId
    , formEmail :: !ByteString
    , formPassword :: !Text
    , formPassword2 :: !Text
    }

def :: Form
def = Form
    { formUserId = Nothing
    , formEmail = ""
    , formPassword = ""
    , formPassword2 = ""
    }

data FormValidationError
    = InvalidEmail
    | EmailNotUnique
    | InvalidPass  !InvalidReason
    | Paswword2Mismatch

validateEmail :: ByteString -> Validation (NonEmpty FormValidationError) Email
validateEmail emailInput = maybeToSuccess (NE.singleton InvalidEmail) $ toEmail <$> EmailV.emailAddress emailInput
    where
        toEmail :: EmailAddress -> Email
        toEmail = Email . decodeUtf8 . EmailV.toByteString

validatePasswd :: Text -> Text -> Validation (NonEmpty FormValidationError) Password
validatePasswd pass pass2 = validatePassPolicy pass *> validatePassword2 pass pass2

validatePassPolicy :: Text -> Validation (NonEmpty FormValidationError) Password
validatePassPolicy inputPass = mkPassword inputPass <$ checkFailure result
    where
        result = validatePassword defaultPasswordPolicy_ $ mkPassword inputPass
        checkFailure :: ValidationResult -> Validation (NonEmpty FormValidationError) ()
        checkFailure ValidPassword = Success ()
        checkFailure (InvalidPassword errs) = case nonEmpty (fmap InvalidPass errs) of
            Nothing -> Success ()
            Just errs' -> Failure errs'

validatePassword2 :: Text -> Text -> Validation (NonEmpty FormValidationError) Password
validatePassword2 inputPass inputPass2 = mkPassword inputPass <$ failureIf (inputPass /= inputPass2) Paswword2Mismatch

validateForm :: Form -> Validation (NonEmpty FormValidationError) (Email, Password)
validateForm Form{..} = (,)
    <$> validateEmail formEmail
    <*> validatePasswd formPassword formPassword2


users :: ScottyT App ()
users = do
    Scotty.get "/users" listOfUsers
    Scotty.get "/add-user" $ userForm def Nothing
    Scotty.post "/add-user" addUserHandler
    Scotty.get "/user/:id" $ do
        layout <- layoutM
        Scotty.html . renderHtml $ layout (h1 "User added. Congrats!!!!")

listOfUsers :: ActionT App ()
listOfUsers = do
    pool <- lift $ asks connPool
    allUsers <- liftIO $ withResource pool $ \conn -> do
        query_ @User conn stmt
    layout <- layoutM
    Scotty.html . renderHtml $ layout $ do
        h1 "Users"
        a ! href "/add-user" $ "Add new user"
        table $ do
            caption "Total users: 4"
            thead $ tr $ do
                th ! scope "col" $ "#"
                th ! scope "col" $ "Email"
            tbody $ do
                forM_ allUsers toRow
        
    where
        toRow :: User -> Html
        toRow User{..} = tr $ do
            th ! scope "col" $ text . T.pack . show $ userId
            td $ text (un email)
        stmt = [sql|
            SELECT id, email, locked_at, failed_login_attempts FROM users;
        |]

userForm :: Form -> Maybe (NonEmpty FormValidationError) -> ActionT App ()
userForm Form{..} errors = do
    layout <- layoutM
    Scotty.html . renderHtml $ layout $ do
        a ! href "/users" $ "Back"
        h1 "Add user"
        form ! method "POST" $ do
            whenJust formUserId $ \(UserId uid) -> input ! name "id" ! type_ "hidden" ! value (toValue uid)
            label $ do
                "Email"
                input ! required "required" ! name "email" ! type_ "email" ! value (toValue $ decodeUtf8 @Text formEmail)
                whenJust errors $ \errs -> renderEmailErrors errs
            label $ do
                "Password"
                input ! required "required" ! name "password" ! type_ "password" ! value (toValue formPassword)
                whenJust errors $ \errs -> renderPasswordErrors errs
            label $ do
                "Confirm password"
                input ! required "required" ! name "password2" ! type_ "password" ! value (toValue formPassword2)
            button ! type_ "submit" $ "Save"

renderEmailErrors :: NonEmpty FormValidationError -> Html
renderEmailErrors errs = ul $ forM_ errs render
    where
        render :: FormValidationError -> Html
        render InvalidEmail = li "Invalid email address"
        render EmailNotUnique = li "Email already exists"
        render _ = mempty

renderPasswordErrors :: NonEmpty FormValidationError -> Html
renderPasswordErrors errs = ul $ forM_ errs render
    where
        render :: FormValidationError -> Html
        render (InvalidPass reason) =  render' reason
        render Paswword2Mismatch = li "Mismatch with second password field"
        render _ = mempty
        render' :: InvalidReason -> Html
        render' (PasswordTooShort minLen _) = li $ text $ "Password is too short. Min length " +| minLen |+ " characters."
        render' (PasswordTooLong maxLen _) = li $ text $ "Password too long. Maz length " +| maxLen |+ " characters."
        render' (NotEnoughReqChars Uppercase minAmount _) = li $ text $ "At least " +| minAmount |+ " of uppercase letters required."
        render' (NotEnoughReqChars Lowercase minAmount _) = li $ text $ "At least " +| minAmount |+ " of lowercase letters required."
        render' (NotEnoughReqChars Special minAmount _) = li $ text $ "At least " +| minAmount |+ " of special characters required."
        render' (NotEnoughReqChars Digit minAmount _) = li $ text $ "At least " +| minAmount |+ " of digits required."
        render' (InvalidCharacters chars) = li $ "Password contains chracters than cannot be used: " <> text chars

-- TODO: handle unexpected SQL errors
createUser :: AppEnv -> Form -> IO (Either (NonEmpty FormValidationError) UserId)
createUser env formData = case validateForm formData of
    Success data' -> catchViolation catcher $ Right <$> run env data'
    Failure errs -> pure $ Left errs
    where
        catcher _ (UniqueViolation "email_unique") = pure . Left $ NE.singleton EmailNotUnique
        catcher e _ = throwIO e

        run :: AppEnv -> (Email, Password) -> IO UserId
        run AppEnv{..} (mail, pass) = do
            pwHash <- hashPassword pass
            liftIO $ withResource connPool $ \conn -> do
                [Only uid] <- query conn stmt (mail, pwHash)
                return (uid :: UserId)

        stmt = [sql|
            INSERT INTO users (email, password) VALUES (?, ?) RETURNING id
        |]

addUserHandler :: ActionT App ()
addUserHandler = do
    formData <- Form Nothing
        <$> Scotty.formParam "email"
        <*> Scotty.formParam "password"
        <*> Scotty.formParam "password2"
    env <- lift ask
    result <- liftIO $ createUser env formData
    either (handleError formData) handleSucces result
    where
        handleSucces :: UserId -> ActionT App ()
        handleSucces userId = Scotty.redirect $ "/user/" +| unUserId userId |+ ""

        handleError :: Form -> NonEmpty FormValidationError -> ActionT App ()
        handleError f errs = do
            Scotty.status badRequest400
            userForm f $ Just errs

