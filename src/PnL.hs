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
import qualified Text.Blaze.Html5.Attributes as Attr
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
import Relude.Extra (un)
import Database.PostgreSQL.Simple.FromField (Format(Text))

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
    , transactionYear :: !Integer
    , transactionMonth :: !Int
    }
    deriving (Show, Generic)
instance FromRow Transaction where
    fromRow = Transaction <$> field <*> field <*> field <*> field <*> field

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

prevPeriod :: Period -> Period
prevPeriod (Period (y, m))
    | m > 1    = Period (y, m - 1)
    | otherwise = Period (y - 1, 12)

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
    , formYear :: !Integer
    , formMonth :: !Int
    }

data ValidationError
    = EmptyDescription
    | DescriptionTooLong
    | ValueToHigh
    | ValueToLow
    | InvalidYear
    | InvalidMonth

instance TextShow ValidationError where
    showb EmptyDescription = "Nazwa nie może być pusta"
    showb DescriptionTooLong = "Nazwa nie może być dłuższa niż 256 znaków"
    showb ValueToLow = "Wartość nie może być mniejsza niż " <> showb minValue
    showb ValueToHigh = "Wartość nie może być większa niż " <> showb maxValue
    showb InvalidYear = "Nieprawidłowy rok"
    showb InvalidMonth = "Miesiąc może przyjmować wartość od 1 do 12."

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

validateYear :: Integer -> Validation (NonEmpty FieldError) Integer
validateYear year = year <$ failureIf (year < 2025) (FieldError "year" InvalidYear)

validateMonth :: Int -> Validation (NonEmpty FieldError) Int
validateMonth month = month <$ failureIf (month < 1 && month > 12) (FieldError "month" InvalidMonth)

validateForm :: Form -> Validation (NonEmpty FieldError) Form
validateForm Form {..}
    = Form formId
    <$> validateName formDescription
    <*> validateValue formValue
    <*> validateYear formYear
    <*> validateMonth formMonth

pnl :: ScottyT App ()
pnl = do
    Scotty.get "/pnl" $ ensureSession >> Scotty.rescue defaultTransactions logSqlError
    Scotty.get "/pnl/add-transaction" $ ensureSession >> defaultForm
    Scotty.post "/pnl/add-transaction" $ ensureSession >> addTransactionHandler
    Scotty.get "/pnl/edit-transaction/:id" $ ensureSession >> editTransactionHandler
    Scotty.post "/pnl/edit-transaction/:id" $ ensureSession >> updateTransactionHandler
    Scotty.delete "/pnl/delete-transaction/:id" $ ensureSession >> deleteTransactionHandler
    Scotty.get "/pnl/transactions/:y/:m" $ ensureSession >> periodTransactions
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

defaultTransactions :: ActionT App ()
defaultTransactions = do
    period <- liftIO currentPeriod
    transactions period

periodTransactions :: ActionT App ()
periodTransactions = do
    y <- Scotty.captureParam @Integer "y"
    m <- Scotty.captureParam @Int "m"
    transactions $ Period (y, m)

transactions :: Period -> ActionT App ()
transactions period@(Period (y, m)) = do
    pool <- lift $ asks connPool
    allTransactions <- liftIO $ withResource pool $ \conn -> do
        query conn stmt (y, m)
    layout <- layoutM
    let total = sum $ transactionValue <$> allTransactions
        Period (prevY, prevM) = prevPeriod period
        Period (nextY, nextM) = nextPeriod period
    Scotty.html . renderHtml $ layout $ do
        header $ do
            a ! href ("/pnl/transactions/" <> toValue prevY <> "/" <> toValue prevM) $ button "<<"
            h1 $ text $ renderPeriod period
            a ! href ("/pnl/transactions/" <> toValue nextY <> "/" <> toValue nextM) $ button ">>"
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
            SELECT id, name, value, year, month FROM transactions WHERE year = ? AND month = ? ORDER BY id DESC;
        |]

addTransactionHandler :: ActionT App ()
addTransactionHandler = do
    formData <- Form Nothing
        <$> Scotty.formParam "description"
        <*> Scotty.formParam "value"
        <*> Scotty.formParam "year"
        <*> Scotty.formParam "month"
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
            [Only tid] <- query conn stmt (formDescription, formValue, formYear, formMonth)
            return $ Right (tid :: TransactionId)
        Failure errs -> return $ Left errs
    where
        stmt = [sql|
            INSERT INTO transactions (name, value, created_at, year, month) VALUES (?, ?, transaction_timestamp(), ?, ?) RETURNING id;
        |]

editTransactionHandler :: ActionT App ()
editTransactionHandler = do
    paramId <- TransactionId <$> Scotty.captureParam @Int64 "id"
    env <- lift ask
    Transaction{..} <- liftIO $ getFromDb env paramId
    let formData = Form
                    (Just transactionId)
                    description
                    (Scientific.toRealFloat transactionValue)
                    transactionYear
                    transactionMonth
    transactionForm formData Nothing
    where
        getFromDb :: AppEnv -> TransactionId -> IO Transaction
        getFromDb AppEnv{..} paramId = do
            withResource connPool $ \conn -> do
                [entity] <- query conn selectStmt $ Only paramId
                return entity
        selectStmt = [sql|
            SELECT id, name, value, year, month FROM transactions WHERE id = ?;
        |]

updateTransactionHandler :: ActionT App ()
updateTransactionHandler = do
    formData <- (Form . Just . TransactionId <$> Scotty.formParam "id")
        <*> Scotty.formParam "description"
        <*> Scotty.formParam "value"
        <*> Scotty.formParam "year"
        <*> Scotty.formParam "month"
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
updateTransaction _ (Form Nothing _ _ _ _) = error "Can't update"
updateTransaction AppEnv{..} formData@(Form (Just transId) _ _ _ _) = do
    case validateForm formData of
        Success Form{..} -> withResource connPool $ \conn -> do
            _ <- execute conn stmt (formDescription, formValue, transId)
            return $ Right ()
        Failure errs -> return $ Left errs
    where
        stmt = [sql|
            UPDATE transactions SET name = ?, value = ? WHERE id = ?;
        |]

defaultForm :: ActionT App ()
defaultForm = do
    (y, m) <- un @(Year, MonthOfYear) <$> liftIO currentPeriod
    transactionForm (Form Nothing "" 0 y m) Nothing

transactionForm :: Form -> Maybe (NonEmpty FieldError) -> ActionT App ()
transactionForm Form{..} errors = do
    layout <- layoutM
    Scotty.html . renderHtml $ layout $ do
        a ! href "/pnl" $ "Wróć"
        h1 "Nowa transakcja"
        form ! method "POST" $ do
            whenJust formId $ \tId -> input ! name "id" ! type_ "hidden" ! value (toAttrVal tId)
            label $ do
                "Rok"
                inputYear formYear formId
            label $ do
                "Miesiąc"
                selectMonths formMonth formId
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

        inputYear :: Integer -> Maybe TransactionId -> Html
        inputYear y maybeId = input
            ! type_ "number"
            ! value (toValue y)
            ! name "year"
            ! Attr.min "2025"
            ! Attr.max "2035"
            ! step "1"
            !? (isJust maybeId, readonly "true")

        selectMonths :: Int -> Maybe TransactionId -> Html
        selectMonths defValue maybeId = select ! name "month" ! value (toValue defValue) !? (isJust maybeId, readonly "true") $ do
            option ! value "1" !? (defValue == 1, selected "true") $ "Styczeń"
            option ! value "2" !? (defValue == 2, selected "true") $ "Luty"
            option ! value "3" !? (defValue == 3, selected "true") $ "Marzec"
            option ! value "4" !? (defValue == 4, selected "true") $ "Kwiecień"
            option ! value "5" !? (defValue == 5, selected "true") $ "Maj"
            option ! value "6" !? (defValue == 6, selected "true") $ "Czerwiec"
            option ! value "7" !? (defValue == 7, selected "true") $ "Lipiec"
            option ! value "8" !? (defValue == 8, selected "true") $ "Sierpień"
            option ! value "9" !? (defValue == 9, selected "true") $ "Wrzesień"
            option ! value "10" !? (defValue == 10, selected "true") $ "Październik"
            option ! value "11" !? (defValue == 11, selected "true") $ "Listopad"
            option ! value "12" !? (defValue == 12, selected "true") $ "Grudzień"

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
