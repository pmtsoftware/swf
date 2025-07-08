{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PnL
    ( pnl
    ) where

import Common
import Homepage

import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT, ActionT)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text
import TextShow
import Database.PostgreSQL.Simple.Time (ZonedTimestamp)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Relude.Extra.Newtype (un)
import Homepage (layout)

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
    { transactionId :: TransactionId
    , description :: !Text
    , value :: !Value
    }
    deriving (Show, Generic)
instance FromRow Transaction where
    fromRow = Transaction <$> field <*> field <*> field

pnl :: ScottyT App ()
pnl = do
    Scotty.get "/pnl" transactions
    Scotty.get "/add-transaction" addTransactionForm
    Scotty.post "/add-transaction" undefined

transactions :: ActionT App ()
transactions = do
    pool <- lift $ asks connPool
    allTransactions <- liftIO $ withResource pool $ \conn -> do
        query_ @Transaction conn stmt
    Scotty.html . renderHtml $ markup allTransactions
    where
        toRow :: Transaction -> Html
        toRow Transaction{..} = tr $ do
            th ! scope "col" $ text (showt transactionId)
            td $ text description
            td $ text (showt value)
        markup :: [Transaction] -> Html
        markup ts = layout $ do
            h1 "Lipiec, 2025"
            a ! href "/add-transaction" $ "Dodaj"
            table $ do
                thead $ tr $ do
                    th ! scope "col" $ "#"
                    th ! scope "col" $ "Nazwa"
                    th ! scope "col" $ "Wartość"
                tbody $ do
                    forM_ ts toRow
        stmt = [sql|
            SELECT id, name, value FROM transactions;
        |]

addTransactionForm :: ActionT App ()
addTransactionForm = Scotty.html . renderHtml $ markup
    where
        markup = layout $ do
            a ! href "/pnl" $ "Wróć"
            h1 "Nowa transakcja"
            form ! method "POST" $ do
                label $ do
                    "Nazwa"
                    textarea ! required "required" ! name "description" $ mempty
                label $ do
                    "Wartość"
                    input ! required "required" ! name "value" ! type_ "number"
                button ! type_ "submit" $ "Save"


addTransaction :: ActionT App ()
addTransaction = do
    description <- Scotty.formParam @Text "description"
    val <- Value <$> Scotty.formParam @Double "value"
    AppEnv{..} <- lift ask
    transactionId <- liftIO $ withResource connPool $ \conn -> do
        [Only tid] <- query conn stmt (description, val)
        return (tid :: TransactionId)
    Scotty.redirect $ "/pnl"
    where
        stmt = [sql|
            INSERT INTO transactions (name, value, created_at) VALUES (?, ?, transaction_timestamp()) RETURNING id
        |]
