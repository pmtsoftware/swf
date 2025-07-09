{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PnL
    ( pnl
    ) where

import Common

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
import Session (ensureSession)

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

pnl :: ScottyT App ()
pnl = do
    Scotty.get "/pnl" $ ensureSession >> Scotty.rescue transactions logSqlError
    Scotty.get "/add-transaction" $ ensureSession >> addTransactionForm
    Scotty.post "/add-transaction" $ ensureSession >> addTransaction
    Scotty.get "/error" errorPage

logSqlError :: SomePostgreSqlException -> ActionT App ()
logSqlError e = do
    lift $ logErrorN $ toText $ displayException e
    Scotty.redirect "/error"

errorPage :: ActionT App ()
errorPage = do
    layout <- layoutM
    Scotty.html . renderHtml . layout $ h1 "Błąd"

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

addTransactionForm :: ActionT App ()
addTransactionForm = do
    layout <- layoutM
    Scotty.html . renderHtml $ layout $ do
        a ! href "/pnl" $ "Wróć"
        h1 "Nowa transakcja"
        form ! method "POST" $ do
            label $ do
                "Nazwa"
                textarea ! required "required" ! name "description" $ mempty
            label $ do
                "Wartość"
                input ! required "required" ! name "value" ! type_ "number" ! step "0.01"
            button ! type_ "submit" $ "Save"


addTransaction :: ActionT App ()
addTransaction = do
    description <- Scotty.formParam @Text "description"
    val <- Value <$> Scotty.formParam @Double "value"
    AppEnv{..} <- lift ask
    _ <- liftIO $ withResource connPool $ \conn -> do
        [Only tid] <- query conn stmt (description, val)
        return (tid :: TransactionId)
    Scotty.redirect "/pnl"
    where
        stmt = [sql|
            INSERT INTO transactions (name, value, created_at) VALUES (?, ?, transaction_timestamp()) RETURNING id
        |]
