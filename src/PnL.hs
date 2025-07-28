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
import Network.HTTP.Types (ok200)
import Data.Time (Year, MonthOfYear, getCurrentTime, UTCTime (utctDay, UTCTime), toGregorian, fromGregorian)

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

newtype Period = Period (Year, MonthOfYear)
    deriving (Eq, Show)

renderPeriod :: Period -> Text
renderPeriod (Period (y, m)) = renderMonth m <> ", " <> showt y
    where
        renderMonth :: MonthOfYear -> Text
        renderMonth 1 = "Styczeń"
        renderMonth 2 = "Luty"
        renderMonth 3 = "Marzec"
        renderMonth 4 = "Kwiecień"
        renderMonth 5 = "Maj"
        renderMonth 6 = "Czerwiec"
        renderMonth 7 = "Lipiec"
        renderMonth 8 = "Sierpień"
        renderMonth 9 = "Wrzesień"
        renderMonth 10 = "Październik"
        renderMonth 11 = "Listopad"
        renderMonth 12 = "Grudzień"
        renderMonth _ = "Unknown"

nextPeriod :: Period -> Period
nextPeriod (Period (y, m))
    | m < 12    = Period (y, m + 1)
    | otherwise = Period (y + 1, 1)

currentPeriod :: IO Period
currentPeriod = do
    (y, m, _) <- toGregorian . utctDay <$> getCurrentTime
    return $ Period (y, m)

periodRange :: Period -> (UTCTime, UTCTime)
periodRange period@(Period (y, m)) = (pstart ,pend)
    where
        Period (y', m') = nextPeriod period
        pstart = UTCTime (fromGregorian y m 1) 0
        pend = UTCTime (fromGregorian y' m' 1) 0

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
        validateNameNotEmpty x = x <$ failureIf (T.null fName) (FieldError "description" EmptyDescription)
        validateNameTooLong x = x <$ failureIf (T.length fName > 256) (FieldError "description" DescriptionTooLong)

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
    Scotty.get "/pnl/add-transaction" $ ensureSession >> transactionForm (Form Nothing "" 0) Nothing
    Scotty.post "/pnl/add-transaction" $ ensureSession >> addTransactionHandler
    Scotty.get "/pnl/edit-transaction/:id" $ ensureSession >> editTransactionHandler
    Scotty.post "/pnl/edit-transaction/:id" $ ensureSession >> updateTransactionHandler
    Scotty.delete "/pnl/delete-transaction/:id" $ ensureSession >> deleteTransactionHandler
    Scotty.get "/pnl/success" successPage
    Scotty.get "/pnl/transaction-deleted" deletedPage
    Scotty.get "/pnl/error" errorPage

logSqlError :: SomePostgreSqlException -> ActionT App ()
logSqlError e = do
    lift $ logErrorN $ toText $ displayException e
    Scotty.redirect "/pnl/error"

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
            li $ a ! href "/pnl/add-transaction" $ "Dodaj kolejną"
            li $ a ! href "/pnl" $ "Wróć do listy"

deletedPage :: ActionT App ()
deletedPage = do
    layout <- layoutM
    Scotty.html . renderHtml . layout $ do
        h1 "Transakcja usunięta"
        ul $ do
            li $ a ! href "/pnl" $ "Wróć do listy"

printScientificValue :: Scientific -> Text
printScientificValue = toText . Scientific.formatScientific Scientific.Generic (Just 2)

transactions :: ActionT App ()
transactions = do
    pool <- lift $ asks connPool
    period <- liftIO currentPeriod
    allTransactions <- liftIO $ withResource pool $ \conn -> do
        query conn stmt $ periodRange period
    layout <- layoutM
    let total = sum $ transactionValue <$> allTransactions
    Scotty.html . renderHtml $ layout $ do
        h1 $ text $ renderPeriod period
        a ! href "/pnl/add-transaction" $ "Dodaj"
        table ! id "transactions" $ do
            thead $ tr $ do
                th ! scope "col" $ "#"
                th ! scope "col" $ "Nazwa"
                th ! scope "col" $ "Wartość"
                th mempty
            tbody $ do
                forM_ allTransactions toRow
                tfoot $ do
                    th ! scope "row" ! colspan "2" $ "Suma:"
                    th $ text (printScientificValue total)
                    th mempty
    where
        editLink :: TransactionId -> Html
        editLink tId = a ! href ("/pnl/edit-transaction/" <> toValue (showt tId)) $ "Edytuj"
        toRow :: Transaction -> Html
        toRow Transaction{..} = tr $ do
            th ! scope "col" $ text (showt transactionId)
            td $ text description
            td $ text (printScientificValue transactionValue)
            td $ editLink transactionId
        stmt = [sql|
            SELECT id, name, value FROM transactions WHERE created_at >= ? AND created_at < ? ORDER BY id DESC;
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
        handleSucces _ = Scotty.redirect "/pnl/success"

        handleError :: Form -> NonEmpty FieldError -> ActionT App ()
        handleError f errs = do
            -- htmx does not swap content if response ststus is 4*/5*
            -- Scotty.status badRequest400
            transactionForm f $ Just errs

addTransaction :: AppEnv -> Form -> IO (Either (NonEmpty FieldError) TransactionId)
addTransaction AppEnv{..} formData = do
    case validateForm formData of
        Success Form{..} -> withResource connPool $ \conn -> do
            [Only tid] <- query conn stmt (formDescription, formValue)
            return $ Right (tid :: TransactionId)
        Failure errs -> return $ Left errs
    where
        stmt = [sql|
            INSERT INTO transactions (name, value, created_at) VALUES (?, ?, transaction_timestamp()) RETURNING id
        |]

editTransactionHandler :: ActionT App ()
editTransactionHandler = do
    paramId <- TransactionId <$> Scotty.captureParam @Int64 "id"
    env <- lift ask
    Transaction{..} <- liftIO $ getFromDb env paramId
    let formData = Form (Just transactionId) description $ Scientific.toRealFloat transactionValue
    transactionForm formData Nothing
    where
        getFromDb :: AppEnv -> TransactionId -> IO Transaction
        getFromDb AppEnv{..} paramId = do
            withResource connPool $ \conn -> do
                [entity] <- query conn selectStmt $ Only paramId
                return entity
        selectStmt = [sql|
            SELECT id, name, value FROM transactions WHERE id = ?;
        |]

updateTransactionHandler :: ActionT App ()
updateTransactionHandler = do
    formData <- (Form . Just . TransactionId <$> Scotty.formParam "id")
        <*> Scotty.formParam "description"
        <*> Scotty.formParam "value"
    env <- lift ask
    result <- liftIO $ updateTransaction env formData
    either (handleError formData) (const (handleSucces formData)) result
    where
        handleSucces :: Form -> ActionT App ()
        handleSucces fd = transactionForm fd Nothing

        handleError :: Form -> NonEmpty FieldError -> ActionT App ()
        handleError f errs = do
            -- TODO: change htmx config
            -- htmx does not swap content if response ststus is 4*/5*
            -- Scotty.status badRequest400
            transactionForm f $ Just errs

updateTransaction :: AppEnv -> Form -> IO (Either (NonEmpty FieldError) ())
updateTransaction _ (Form Nothing _ _) = error "Can't update"
updateTransaction AppEnv{..} formData@(Form (Just transId) _ _) = do
    case validateForm formData of
        Success Form{..} -> withResource connPool $ \conn -> do
            _ <- execute conn stmt (formDescription, formValue, transId)
            return $ Right ()
        Failure errs -> return $ Left errs
    where
        stmt = [sql|
            UPDATE transactions SET name = ?, value = ? WHERE id = ?;
        |]

transactionForm :: Form -> Maybe (NonEmpty FieldError) -> ActionT App ()
transactionForm Form{..} errors = do
    layout <- layoutM
    Scotty.html . renderHtml $ layout $ do
        a ! href "/pnl" $ "Wróć"
        h1 "Nowa transakcja"
        form ! method "POST" $ do
            whenJust formId $ \tId -> input ! name "id" ! type_ "hidden" ! value (toAttrVal tId)
            label $ do
                "Nazwa"
                textarea ! required "required" ! name "description" $ text formDescription
                whenJust errors $ renderErrors "description"
            label $ do
                "Wartość"
                input ! required "required" ! name "value" ! type_ "number" ! step "0.01" ! value (toValue formValue)
                whenJust errors $ renderErrors "value"
            button ! type_ "submit" $ "Save"
        whenJust formId $ \tid -> deleteLink tid
    where
        renderErrors :: Text -> NonEmpty FieldError -> Html
        renderErrors n es = flip whenNotNull renderErrors' $ NE.filter (\FieldError{..} -> fName == n) es

        renderErrors' :: NonEmpty FieldError -> Html
        renderErrors' errs = ul $ forM_ errs (li . text . showt . fError)

        deleteLink :: TransactionId -> Html
        deleteLink tid = a
            ! class_ "delete"
            ! href "#"
            ! customAttribute "hx-delete" ("/pnl/delete-transaction/" <> toAttrVal tid)
            ! customAttribute "hx-target" "main"
            ! customAttribute "hx-confirm" "Czy na pewno chcesz usunąć?"
            $ "Usuń"

deleteTransactionHandler :: Handler ()
deleteTransactionHandler = do
    env <- lift ask
    transactionId <- TransactionId <$> Scotty.captureParam "id"
    liftIO $ deleteTransaction env transactionId
    Scotty.status ok200
    Scotty.html . renderHtml $ do
        h1 "Transakcja usunięta"
        ul $ do
            li $ a ! href "/pnl" $ "Wróć do listy"

deleteTransaction :: AppEnv -> TransactionId -> IO ()
deleteTransaction AppEnv{..} transactionId = do
    withResource connPool $ \conn -> do
        _ <- execute conn stmt $ Only transactionId
        return ()
    where
        stmt = [sql|
            DELETE FROM transactions WHERE id = ?;
        |]

toAttrVal :: (TextShow a) => a -> AttributeValue
toAttrVal = toValue . showt
