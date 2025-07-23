{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PnL
    ( pnl
    ) where

import Common hiding (Fixed)

import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT, ActionT)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text
import TextShow hiding (toText)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Homepage (layoutM)
import Control.Monad.Logger (logErrorN)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Session (ensureSession)
import Validation

newtype TransactionId = TransactionId Int64
    deriving (Show, Eq, Generic)
deriving newtype instance FromField TransactionId
deriving newtype instance ToField TransactionId
deriving newtype instance TextShow TransactionId

newtype Value = Value { unValue :: Double }
    deriving (Show, Eq, Generic)
deriving newtype instance FromField Value
deriving newtype instance ToField Value
deriving newtype instance TextShow Value

data Transaction = Transaction
    { transactionId :: !TransactionId
    , description :: !Text
    , transactionValue :: !Scientific
    }
    deriving (Show, Generic)
instance FromRow Transaction where
    fromRow = Transaction <$> field <*> field <*> field

data Form = Form
    { formId :: !(Maybe TransactionId)
    , formDescription :: !Text
    , formValue :: !Double
    }

data ValidationError
    = EmptyDescription
    | DescriptionTooLong
    | ValueToHigh
    | ValueToLow

instance TextShow ValidationError where
    showb EmptyDescription = "Nazwa nie może być pusta"
    showb DescriptionTooLong = "Nazwa nie może być dłuższa niż 256 znaków"
    showb ValueToLow = "Wartość nie może być mniejsza niż " <> showb minValue
    showb ValueToHigh = "Wartość nie może być większa niż " <> showb maxValue

data FieldError = FieldError
    { fName    :: !Text
    , fError   :: !ValidationError
    }

maxValue :: Double
maxValue = 999999.99

minValue :: Double
minValue = -999999.99

validateName :: Text -> Validation (NonEmpty FieldError) Text
validateName fName = validateNameNotEmpty fName <* validateNameTooLong fName
    where
        validateNameNotEmpty x = x <$ failureIf (T.null fName) (FieldError "descrition" EmptyDescription)
        validateNameTooLong x = x <$ failureIf (T.length fName > 256) (FieldError "descrition" DescriptionTooLong)

validateValue :: Double -> Validation (NonEmpty FieldError) Double
validateValue val = validateMaxValue val <* validateMinValue val
    where
        validateMaxValue val' = val' <$ failureIf (val' > maxValue) (FieldError "value" ValueToHigh)
        validateMinValue val' = val' <$ failureIf (val' < minValue) (FieldError "value" ValueToLow)

validateForm :: Form -> Validation (NonEmpty FieldError) Form
validateForm Form {..} = Form formId <$> validateName formDescription <*> validateValue formValue

pnl :: ScottyT App ()
pnl = do
    Scotty.get "/pnl" $ ensureSession >> Scotty.rescue transactions logSqlError
    Scotty.get "/add-transaction" $ ensureSession >> addTransactionForm (Form Nothing "" 0) Nothing
    Scotty.post "/add-transaction" $ ensureSession >> addTransactionHandler
    Scotty.get "/success" successPage
    Scotty.get "/error" errorPage

logSqlError :: SomePostgreSqlException -> ActionT App ()
logSqlError e = do
    lift $ logErrorN $ toText $ displayException e
    Scotty.redirect "/error"

errorPage :: ActionT App ()
errorPage = do
    layout <- layoutM
    Scotty.html . renderHtml . layout $ h1 "Błąd"

successPage :: ActionT App ()
successPage = do
    layout <- layoutM
    Scotty.html . renderHtml . layout $ do
        h1 "Transakcja zapisana"
        ul $ do
            li $ a ! href "/add-transaction" $ "Dodaj kolejną"
            li $ a ! href "/pnl" $ "Wróć do listy"

printScientificValue :: Scientific -> Text
printScientificValue = toText . Scientific.formatScientific Scientific.Generic (Just 2)

transactions :: ActionT App ()
transactions = do
    pool <- lift $ asks connPool
    allTransactions <- liftIO $ withResource pool $ \conn -> do
        query_ @Transaction conn stmt
    layout <- layoutM
    let total = sum $ transactionValue <$> allTransactions
    Scotty.html . renderHtml $ layout $ do
        h1 "Lipiec, 2025"
        a ! href "/add-transaction" $ "Dodaj"
        table ! id "transactions" $ do
            thead $ tr $ do
                th ! scope "col" $ "#"
                th ! scope "col" $ "Nazwa"
                th ! scope "col" $ "Wartość"
            tbody $ do
                forM_ allTransactions toRow
                tfoot $ do
                    th ! scope "row" ! colspan "2" $ "Suma:"
                    th $ text (printScientificValue total)
    where
        toRow :: Transaction -> Html
        toRow Transaction{..} = tr $ do
            th ! scope "col" $ text (showt transactionId)
            td $ text description
            td $ text (printScientificValue transactionValue)
        stmt = [sql|
            SELECT id, name, value FROM transactions ORDER BY id DESC;
        |]

addTransactionHandler :: ActionT App ()
addTransactionHandler = do
    formData <- Form Nothing
        <$> Scotty.formParam "description"
        <*> Scotty.formParam "value"
    env <- lift ask
    result <- liftIO $ addTransaction env formData
    either (handleError formData) handleSucces result
    where
        handleSucces :: TransactionId -> ActionT App ()
        handleSucces _ = Scotty.redirect "/success"

        handleError :: Form -> NonEmpty FieldError -> ActionT App ()
        handleError f errs = do
            -- htmx does not swap content if response ststus is 4*/5*
            -- Scotty.status badRequest400
            addTransactionForm f $ Just errs

addTransaction :: AppEnv -> Form -> IO (Either (NonEmpty FieldError) TransactionId)
addTransaction AppEnv{..} formData = do
    case validateForm formData of
        Success Form{..} -> withResource connPool $ \conn -> do
            [Only tid] <- query conn stmt (formDescription, formValue)
            return $ Right (tid :: TransactionId)
        Failure errs -> return $ Left errs
    -- Scotty.redirect "/pnl"
    where
        stmt = [sql|
            INSERT INTO transactions (name, value, created_at) VALUES (?, ?, transaction_timestamp()) RETURNING id
        |]

addTransactionForm :: Form -> Maybe (NonEmpty FieldError) -> ActionT App ()
addTransactionForm Form{..} errors = do
    layout <- layoutM
    Scotty.html . renderHtml $ layout $ do
        a ! href "/pnl" $ "Wróć"
        h1 "Nowa transakcja"
        form ! method "POST" $ do
            label $ do
                "Nazwa"
                textarea ! required "required" ! name "description" $ text formDescription
                whenJust errors $ renderErrors "description"
            label $ do
                "Wartość"
                input ! required "required" ! name "value" ! type_ "number" ! step "0.01" ! value (toValue formValue)
                whenJust errors $ renderErrors "value"
            button ! type_ "submit" $ "Save"
    where
        renderErrors :: Text -> NonEmpty FieldError -> Html
        renderErrors n es = flip whenNotNull renderErrors' $ NE.filter (\FieldError{..} -> fName == n) es

        renderErrors' :: NonEmpty FieldError -> Html
        renderErrors' errs = ul $ forM_ errs (li . text . showt . fError)

