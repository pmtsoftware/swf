{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Webauthn.Database
    (
    -- * User
    insertUser,
    userExists,

    -- * Credential Entry
    insertCredentialEntry,
    queryCredentialEntryByCredential,
    queryCredentialEntriesByUser,

    -- * Auth token
    AuthToken (..),
    generateAuthToken,
    insertAuthToken,
    queryUserByAuthToken,
    deleteAuthToken,
    updateSignatureCounter,
) where

import Relude

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Exception (throwIO)
import Crypto.Random (MonadRandom, getRandomBytes)
import qualified Crypto.WebAuthn as WA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Char8 (unpack)
import Database.PostgreSQL.Simple hiding (fold)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.Types as SQL
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (signed, decimal)
import Types

instance FromField Word32 where
    fromField f Nothing = returnError UnexpectedNull f ""
    fromField f (Just bs) = case parseWord32 bs of
        Left _ -> returnError ConversionFailed f (unpack bs)
        Right v -> return v
        where
            parseWord32 :: ByteString -> Either String Word32
            parseWord32 = parseOnly $ signed decimal <|> fail "Invalid Word32"

-- | Inserts a new user into the database. Used during registration.
insertUser ::
    Connection ->
    WA.CredentialUserEntity ->
    IO Int64
insertUser conn user =
    let WA.CredentialUserEntity
            { WA.cueId = WA.UserHandle handle,
              WA.cueName = WA.UserAccountName accountName,
              WA.cueDisplayName = WA.UserAccountDisplayName accountDisplayName
            } = user
        binaryHandle = SQL.Binary handle
    in execute
        conn
        [sql| INSERT INTO users (handle, email, display_name, password) VALUES (?, ?, ?, ''); |]
        (binaryHandle, accountName, accountDisplayName)

-- | Check if a user exists in the database
userExists :: Connection -> WA.UserAccountName -> IO Bool
userExists conn (WA.UserAccountName accountName) = do
  results :: [Only Text] <- query conn [sql| SELECT email FROM users WHERE email = ?; |] $ Only accountName
  pure $ not $ null results

-- | Inserts a new credential entry into the database. The example server's
-- logic doesn't allow multiple credential per user, but a typical RP
-- implementation will likely want to support it.
insertCredentialEntry ::
    Connection ->
    WA.CredentialEntry ->
    IO Int64
insertCredentialEntry
    conn
    WA.CredentialEntry
        { WA.ceUserHandle = WA.UserHandle userHandle,
        WA.ceCredentialId = WA.CredentialId credentialId,
        WA.cePublicKeyBytes = WA.PublicKeyBytes publicKey,
        WA.ceSignCounter = WA.SignatureCounter signCounter,
        WA.ceTransports = encodeTransports -> transports
        } =
    do
        let binaryCredId = SQL.Binary credentialId
            binaryUserHandle = SQL.Binary userHandle
            binaryPubKey = SQL.Binary publicKey
            binaryTransports = SQL.Binary transports
        execute
            conn
            [sql| 
                INSERT INTO credential_entries (credential_id, user_handle, public_key, sign_counter, transports, created_at)
                VALUES (?, ?, ?, ?, ?, transaction_timestamp());
            |]
            ( binaryCredId,
              binaryUserHandle,
              binaryPubKey,
              signCounter,
              binaryTransports
            )

-- | Find a credential entry in the database
queryCredentialEntryByCredential :: Connection -> WA.CredentialId -> IO (Maybe WA.CredentialEntry)
queryCredentialEntryByCredential conn (WA.CredentialId credentialId) = do
    let binaryCredId = SQL.Binary credentialId
    entries <- query
        conn
        sqlStmt
            (Only binaryCredId)
    case entries of
        [] -> pure Nothing
        [entry] -> Just <$> toCredentialEntry entry
        _ -> fail "Unreachable: credential_entries.credential_id has a unique index."
    where
        sqlStmt = [sql| SELECT credential_id, user_handle, public_key, sign_counter, transports FROM credential_entries WHERE credential_id = ?; |]

-- | Retrieve the credential entries belonging to the specified user. In
-- reality, the logic of the server doesn't actually allow a single user to
-- register multiple credentials.
queryCredentialEntriesByUser :: Connection -> WA.UserAccountName -> IO [WA.CredentialEntry]
queryCredentialEntriesByUser conn (WA.UserAccountName accountName) = do
    entries <- query
        conn
        [sql|
            SELECT c.credential_id, c.user_handle, c.public_key, c.sign_counter, c.transports
            FROM credential_entries AS c 
            JOIN users AS u on u.handle = c.user_handle 
            WHERE u.email = ?;
        |]
        (Only accountName)
    traverse toCredentialEntry entries

-- | Set the new signature counter for the specified credential. Used to check
-- if the authenticator wasn't cloned.
updateSignatureCounter :: Connection -> WA.CredentialId -> WA.SignatureCounter -> IO Int64
updateSignatureCounter conn (WA.CredentialId credentialId) (WA.SignatureCounter counter) = do
    let binaryCredId = SQL.Binary credentialId
    execute
        conn
        [sql| UPDATE credential_entries SET sign_counter = ? WHERE credential_id = ?; |]
        (counter, binaryCredId)

-- | Encodes a list of 'WA.AuthenticatorTransport' into a 'BS.ByteString' using
-- CBOR format. Use 'decodeTransports' to inverse this operation. This is only
-- done for simplicity, better might be to store all values in a database table
encodeTransports :: [WA.AuthenticatorTransport] -> BS.ByteString
encodeTransports transports = LBS.toStrict $ serialise $ map WA.encodeAuthenticatorTransport transports

-- | Decodes a 'BS.ByteString' created by 'encodeTransports' into a list of
-- 'WA.AuthenticatorTransport'.
decodeTransports :: BS.ByteString -> IO [WA.AuthenticatorTransport]
decodeTransports bytes = case deserialiseOrFail $ LBS.fromStrict bytes of
  Left err -> throwIO err
  Right result -> pure $ WA.decodeAuthenticatorTransport <$> result

toCredentialEntry :: (BS.ByteString, BS.ByteString, BS.ByteString, Word32, BS.ByteString) -> IO WA.CredentialEntry
toCredentialEntry (credentialId, userHandle, publicKey, signCounter, transportBytes) = do
  transports <- decodeTransports transportBytes
  pure
    WA.CredentialEntry
      { WA.ceCredentialId = WA.CredentialId credentialId,
        WA.ceUserHandle = WA.UserHandle userHandle,
        WA.cePublicKeyBytes = WA.PublicKeyBytes publicKey,
        WA.ceSignCounter = WA.SignatureCounter signCounter,
        WA.ceTransports = transports
      }

newtype AuthToken = AuthToken {unAuthToken :: BS.ByteString}

generateAuthToken :: (MonadRandom m) => m AuthToken
generateAuthToken = AuthToken <$> getRandomBytes 16

-- | Find a user from their `AuthToken` cookie
queryUserByAuthToken :: Connection -> AuthToken -> IO (Maybe WA.UserAccountName)
queryUserByAuthToken conn (AuthToken token) = do
    result <- query
        conn
        [sql| SELECT account_name FROM auth_tokens JOIN users ON users.handle = auth_tokens.user_handle WHERE token = ?; |]
        (Only token)
    case result of
        [] -> pure Nothing
        [Only accountName] -> pure $ Just $ WA.UserAccountName accountName
        _ -> fail "Unreachable: credential_entries.credential_id has a unique index."

-- | Store `AuthToken` to keep the user logged in
insertAuthToken :: Connection -> AuthToken -> WA.UserHandle -> IO Int64
insertAuthToken conn (AuthToken token) (WA.UserHandle userHandle) = do
    execute
        conn
        [sql| INSERT INTO auth_tokens (token, user_handle) VALUES (?, ?); |]
        (token, userHandle)

-- | Remove the `AuthToken` from the database, effectively logging out the
-- user
deleteAuthToken :: Connection -> AuthToken -> IO Int64
deleteAuthToken conn (AuthToken token) = do
    execute
        conn
        [sql| DELETE FROM auth_tokens WHERE token = ?; |]
        [token]
