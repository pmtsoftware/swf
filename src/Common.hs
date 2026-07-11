{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Common
    ( AppEnv(..)
    , App(..)
    , Handler
    , module Relude
    , module Config
    , module Hasql.Pool
    , module Fmt
    , logInfo
    , logDebug
    , logWarn
    , logError
    , runDbSession
    , reportUsageError
    , layoutM
    , withLayout
    ) where

import Relude hiding (div, head, id, span, map)

import Config

import Hasql.Pool
import Hasql.Session (Session, SessionError(..), CommandError(..), ResultError(..), RowError(..))

import UnliftIO (MonadUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logInfoN, logDebugN, logWarnN, logErrorN)
import Web.ClientSession (Key)
import Web.Scotty.Trans (ActionT, raise)
import qualified Web.Scotty.Trans as Scotty
import Fmt ((+|), (|+))
import Webauthn.PendingCeremonies (PendingCeremonies)
import Crypto.WebAuthn (MetadataServiceRegistry, RpIdHash, Origin)

import Text.Blaze.Html5 (Html, Attribute, (!))
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import Text.Blaze.Html.Renderer.Text

data AppEnv = AppEnv
    { cfg :: AppConfig
    , connPool :: Pool
    , sessionKey :: Key
    , cssChecksum :: ByteString
    , pendingCeremonies :: PendingCeremonies
    , registry :: TVar MetadataServiceRegistry
    , rpIdHash :: RpIdHash
    , origin :: Origin
    , dev :: Bool
    }

newtype App a = App { runApp :: ReaderT AppEnv (LoggingT IO) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppEnv, MonadUnliftIO, MonadLogger)

type Handler a = ActionT App a

logInfo :: Text -> Handler ()
logInfo = lift . logInfoN

logDebug :: Text -> Handler ()
logDebug = lift . logDebugN

logWarn :: Text -> Handler ()
logWarn = lift . logWarnN

logError :: Text -> Handler ()
logError = lift . logErrorN

runDbSession :: Session result -> Handler result
runDbSession s = do
    AppEnv{..} <- lift ask
    result <- liftIO $ use connPool s
    case result of
        Left e -> do
            logError $ reportUsageError e
            raise "Operation failed"
        Right r -> pure r

-- | Turn a Hasql 'UsageError' into a readable, structured message for logging.
-- Query parameter values are intentionally omitted (only their count is shown),
-- since they may contain secrets such as password hashes.
reportUsageError :: UsageError -> Text
reportUsageError = \case
    ConnectionUsageError details ->
        "DB connection error"+|detailSuffix details|+""
    AcquisitionTimeoutUsageError ->
        "DB error: timed out acquiring a connection from the pool"
    SessionUsageError sErr -> reportSessionError sErr

reportSessionError :: SessionError -> Text
reportSessionError = \case
    QueryError sql params cmdErr ->
        reportCommandError cmdErr
            |+"\n  statement: "+|decodeUtf8 @Text sql
            |+"\n  parameters: "+|length params|+" value(s)"
    PipelineError cmdErr ->
        "DB pipeline error: "+|reportCommandError cmdErr|+""

reportCommandError :: CommandError -> Text
reportCommandError = \case
    ClientError details -> "DB client error"+|detailSuffix details|+""
    ResultError resErr -> reportResultError resErr

reportResultError :: ResultError -> Text
reportResultError = \case
    ServerError code message detail hint _position ->
        -- code + SQLSTATE name and the optional detail/hint are joined into plain
        -- Text first, so the Fmt chain only interpolates finished values.
        let codeLabel = decodeUtf8 @Text code <> sqlStateSuffix code
            body = decodeUtf8 @Text message
                     <> optSection "\n  detail: " detail
                     <> optSection "\n  hint: " hint
        in "PostgreSQL error "+|codeLabel|+": "+|body|+""
    UnexpectedResult msg -> "DB error: unexpected result: "+|msg|+""
    RowError row col rowErr ->
        "DB error decoding row "+|row|+", column "+|col|+": "+|reportRowError rowErr|+""
    UnexpectedAmountOfRows n ->
        "DB error: unexpected number of rows returned: "+|n|+""

reportRowError :: RowError -> Text
reportRowError = \case
    EndOfInput -> "end of input (fewer columns than expected)"
    UnexpectedNull -> "unexpected NULL"
    ValueError msg -> "value decoding failed: "+|msg|+""

detailSuffix :: Maybe ByteString -> Text
detailSuffix = optSection ": "

-- | Prefix a decoded, optional 'ByteString' with a label, or "" when absent.
optSection :: Text -> Maybe ByteString -> Text
optSection label = maybe "" ((label <>) . decodeUtf8)

-- | Friendly name for the common PostgreSQL SQLSTATE codes we're likely to hit.
sqlStateSuffix :: ByteString -> Text
sqlStateSuffix = \case
    "23505" -> " (unique_violation)"
    "23503" -> " (foreign_key_violation)"
    "23502" -> " (not_null_violation)"
    "23514" -> " (check_violation)"
    "23P01" -> " (exclusion_violation)"
    "40001" -> " (serialization_failure)"
    "40P01" -> " (deadlock_detected)"
    "53300" -> " (too_many_connections)"
    "57014" -> " (query_canceled)"
    _       -> ""


layout :: Bool -> ByteString -> Html -> Html
layout devMode cssChecksum innerHtml = Html.docTypeHtml ! Html.dataAttribute "bs-theme" "dark" $ do
    Html.head $ do
        Html.meta ! Attr.name "viewport" ! Attr.content "width=device-width, initial-scale=1.0"
        Html.title "Simple Web Framework"
        Html.link ! Attr.rel "icon" ! Attr.href "/static/favicon.ico" ! Attr.type_ "image/x-icon"
        Html.link ! Attr.href "/static/matcha.css" ! Attr.rel "stylesheet"
        -- link ! href "https://cdn.jsdelivr.net/npm/water.css@2/out/dark.min.css" ! rel "stylesheet"
        -- link ! href "https://cdn.jsdelivr.net/npm/sakura.css/css/sakura-dark.css" ! rel "stylesheet"
        Html.link ! Attr.href ("/static/swf.css?checksum=" <> checksumAV) ! Attr.rel "stylesheet"
        when devMode $ Html.script ! Attr.type_ "module" ! Attr.src "/static/dev.js" $ mempty
        Html.script ! Attr.src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js" $ mempty
    Html.body ! htmxBoost $ do
        Html.main $ do
            -- navbar
            innerHtml
    where
        checksumAV = Html.toValue . decodeUtf8 @Text $ cssChecksum

layoutM :: Handler (Html -> Html)
layoutM = do
    AppEnv{..} <- lift ask
    return $ layout dev cssChecksum

withLayout :: Html -> Handler ()
withLayout content = do
    l <- layoutM
    Scotty.html . renderHtml $ l content

htmxBoost :: Attribute
htmxBoost = Html.customAttribute "hx-boost" "true"
