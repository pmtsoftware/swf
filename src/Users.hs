{-# LANGUAGE DuplicateRecordFields #-}

module Users ( users ) where

import Common hiding (pass)
import Types
import Homepage (layout)

import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT, ActionT)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text
import qualified Text.Email.Validate as EmailV
import TextShow hiding (toString, toText)
import Data.Password.Argon2 (Password, mkPassword, hashPassword)
import Data.Password.Validate
import Database.PostgreSQL.Simple.Time (ZonedTimestamp)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Relude.Extra.Newtype (un)
import Validation

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
    , formEmail :: Text
    , formPassword :: Text
    , formPassword2 :: Text
    }

def :: Form
def = Form
    { formUserId = Nothing
    , formEmail = ""
    , formPassword = ""
    , formPassword2 = ""
    }

data FormValidationError
    = InvalidEmail !Text
    | InvalidPass  [InvalidReason]
    | Paswword2Mismatch

validateEmail :: Text -> Validation (NonEmpty FormValidationError) Email
validateEmail emailInput = eitherToValidation result
    where
        result = bimap toErr toEmail . EmailV.validate $ encodeUtf8 @Text @ByteString emailInput
        toErr = (:|[]) . InvalidEmail . toText
        toEmail = Email . decodeUtf8 . EmailV.toByteString

validatePasswd :: Text -> Text -> Validation (NonEmpty FormValidationError) Password
validatePasswd pass pass2 = validatePassPolicy pass *> validatePassword2 pass pass2

validatePassPolicy :: Text -> Validation (NonEmpty FormValidationError) Password
validatePassPolicy inputPass = mkPassword inputPass <$ failureIf (isJust result) (InvalidPass (fromMaybe [] result))
    where
        result = getErrs $ validatePassword defaultPasswordPolicy_ $ mkPassword inputPass
        getErrs :: ValidationResult -> Maybe [InvalidReason]
        getErrs ValidPassword = Nothing
        getErrs (InvalidPassword errs) = Just errs

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
    Scotty.post "/add-user" addUser
    Scotty.get "/user/:id" $ do
        Scotty.html . renderHtml $ layout (h1 "User added. Congrats!!!!")

listOfUsers :: ActionT App ()
listOfUsers = do
    pool <- lift $ asks connPool
    allUsers <- liftIO $ withResource pool $ \conn -> do
        query_ @User conn stmt
    Scotty.html . renderHtml $ markup allUsers
    where
        toRow :: User -> Html
        toRow User{..} = tr $ do
            th ! scope "col" $ text (showt userId)
            td $ text (un email)
        markup :: [User] -> Html
        markup us = layout $ do
            h1 "Users"
            a ! href "/add-user" $ "Add new user"
            table $ do
                caption "Total users: 4"
                thead $ tr $ do
                    th ! scope "col" $ "#"
                    th ! scope "col" $ "Email"
                tbody $ do
                    forM_ us toRow
        stmt = [sql|
            SELECT id, email, locked_at, failed_login_attempts FROM users;
        |]

userForm :: Form -> Maybe (NonEmpty FormValidationError) -> ActionT App ()
userForm Form{..} errors = Scotty.html . renderHtml $ markup
    where
        markup = layout $ do
            h1 "Add user"
            form ! method "POST" $ do
                whenJust formUserId $ \(UserId uid) -> input ! name "id" ! type_ "hidden" ! value (toValue uid)
                label $ do
                    "Email"
                    input ! required "required" ! name "email" ! type_ "email" ! value (toValue formEmail)
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
        render (InvalidEmail reason) = li $ "Invalid email: " <> text reason
        render _ = mempty

renderPasswordErrors :: NonEmpty FormValidationError -> Html
renderPasswordErrors errs = ul $ forM_ errs render
    where
        render :: FormValidationError -> Html
        render (InvalidPass rs) = forM_ rs render'
        render Paswword2Mismatch = li "Mismatch with second password field"
        render _ = mempty
        render' :: InvalidReason -> Html
        render' (PasswordTooShort minLen _) = li $ "Password is too short. Min length " <> text (showt minLen) <> " characters."
        render' (PasswordTooLong maxLen _) = li $ "Password too long. Maz length " <> text (showt maxLen) <> " characters."
        render' (NotEnoughReqChars Uppercase minAmount _) = li $ "At least " <> text (showt minAmount) <> " of uppercase letters required."
        render' (NotEnoughReqChars Lowercase minAmount _) = li $ "At least " <> text (showt minAmount) <> " of lowercase letters required."
        render' (NotEnoughReqChars Special minAmount _) = li $ "At least " <> text (showt minAmount) <> " of special characters required."
        render' (NotEnoughReqChars Digit minAmount _) = li $ "At least " <> text (showt minAmount) <> " of digits required."
        render' (InvalidCharacters chars) = li $ "Password contains chracters than cannot be used: " <> text chars

addUser :: ActionT App ()
addUser = do
    formData <- Form Nothing
        <$> Scotty.formParam "email"
        <*> Scotty.formParam "password"
        <*> Scotty.formParam "password2"
    let validated = validateForm formData
    whenSuccess_ validated $ \(email_, pass_) -> do
        pwHash <- hashPassword pass_
        AppEnv{..} <- lift ask
        userId <- liftIO $ withResource connPool $ \conn -> do
            [Only uid] <- query conn stmt (email_, pwHash)
            return (uid :: UserId)
        Scotty.redirect $ "/user/" <> showtl userId
    whenFailure_ validated $ \errs -> do
        userForm formData $ Just errs
    where
        stmt = [sql|
            INSERT INTO users (email, password) VALUES (?, ?) RETURNING id
        |]

