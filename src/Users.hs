{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Users ( users ) where

import Common
import Homepage (layout)

import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT, ActionT)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text
import TextShow
import Data.Password.Argon2 (Password, mkPassword, PasswordHash (..), hashPassword)
import Data.Password.Validate
import Database.PostgreSQL.Simple.Time (ZonedTimestamp)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Relude.Extra.Newtype (un)
import Validation
import Relude.Unsafe (fromJust)

newtype UserId = UserId { unUserId :: Int64 }
    deriving (Show, Eq, Generic)
deriving newtype instance FromField UserId
deriving newtype instance ToField UserId
deriving newtype instance TextShow UserId

newtype Email = Email { unEmail :: Text }
    deriving (Show, Eq, Generic)
deriving newtype instance FromField Email
deriving newtype instance ToField Email

newtype PlainPassword = PlainPassword Text
    deriving (Show, Eq)

newtype PlainPassword2 = PlainPassword2 Text
    deriving (Show, Eq)

deriving instance Generic (PasswordHash a)
deriving newtype instance FromField (PasswordHash a)
deriving newtype instance ToField (PasswordHash a)

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
    { formEmail :: Text
    , formPassword :: Text
    , formPassword2 :: Text
    }

data FormValidationError
    = InvalidEmail !Text
    | InvalidPass  !(NonEmpty Text)
    | Paswword2Mismatch

validatePass :: Text -> Validation (NonEmpty FormValidationError) Password
validatePass input = mkPassword input <$ failureIf (isJust result) undefined
    where
        result = getErrs $ validatePassword defaultPasswordPolicy_ $ mkPassword input

        getErrs :: ValidationResult -> Maybe (NonEmpty FormValidationError)
        getErrs ValidPassword = Nothing
        getErrs (InvalidPassword errs) = nonEmpty $ undefined

        report :: InvalidReason -> Text
        report (PasswordTooShort minLen x) = "Password is too short. Min length " <> showt minLen <> " characters."
        report (PasswordTooLong maxLen x) = "Password too long. Maz length " <> showt maxLen <> " characters."
        report (NotEnoughReqChars Uppercase minAmount x) = "At least " <> showt minAmount <> " of uppercase letters required."
        report (NotEnoughReqChars Lowercase minAmount x) = "At least " <> showt minAmount <> " of lowercase letters required."
        report (NotEnoughReqChars Special minAmount x) = "At least " <> showt minAmount <> " of special characters required."
        report (NotEnoughReqChars Digit minAmount x) = "At least " <> showt minAmount <> " of digits required."
        report (InvalidCharacters chars) = "Password contains chracters than cannot be used: " <> chars

users :: ScottyT App ()
users = do
    Scotty.get "/users" listOfUsers
    Scotty.get "/add-user" addUserForm
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

addUserForm :: ActionT App ()
addUserForm = Scotty.html . renderHtml $ markup
    where
        markup = layout $ do
            h1 "Add user"
            form ! method "POST" $ do
                label $ do
                    "Email"
                    input ! required "required" ! name "email" ! type_ "email"
                label $ do
                    "Password"
                    input ! required "required" ! name "password" ! type_ "password"
                label $ do
                    "Confirm password"
                    input ! required "required" ! name "password2" ! type_ "password"
                button ! type_ "submit" $ "Save"

addUser :: ActionT App ()
addUser = do
    Form{..} <- Form <$> Scotty.formParam "email" <*> Scotty.formParam "password" <*> Scotty.formParam "password2"
    email <- Email <$> Scotty.formParam @Text "email"
    pass <- mkPassword <$> Scotty.formParam @Text "password"
    pass2 <- mkPassword <$> Scotty.formParam @Text "password2"
    pwHash <- hashPassword pass
    AppEnv{..} <- lift ask
    userId <- liftIO $ withResource connPool $ \conn -> do
        [Only uid] <- query conn stmt (email, pwHash)
        return (uid :: UserId)
    Scotty.redirect $ "/user/" <> showtl userId
    where
        stmt = [sql|
            INSERT INTO users (email, password) VALUES (?, ?) RETURNING id
        |]

validate :: Email -> Password -> Password -> Maybe (NonEmpty Text)
validate email pass pass2 = undefined
