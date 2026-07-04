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
import qualified Relude as Rel

import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Exception (throwIO)
import Crypto.Random (MonadRandom, getRandomBytes)
import qualified Crypto.WebAuthn as WA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Data.Vector (Vector)

import Hasql.Statement
import Hasql.Session
import Hasql.TH
import Hasql.Pool
-- import Hasql.Transaction (Transaction)
-- import qualified Hasql.Transaction as Trans
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

-- import Data.Functor.Contravariant ((>$<))
import Data.Profunctor

credentialEntryParams :: E.Params WA.CredentialEntry
credentialEntryParams =
     (WA.unCredentialId   . WA.ceCredentialId   >$< E.param (E.nonNullable E.bytea))
    <> (WA.unUserHandle      . WA.ceUserHandle     >$< E.param (E.nonNullable E.bytea))
    <> (WA.unPublicKeyBytes  . WA.cePublicKeyBytes >$< E.param (E.nonNullable E.bytea))
    <> (fromIntegral . WA.unSignatureCounter . WA.ceSignCounter >$< E.param (E.nonNullable E.int4))
    <> (encodeTransports     . WA.ceTransports     >$< E.param (E.nonNullable E.bytea))

credentialEntryRow :: D.Row WA.CredentialEntry
credentialEntryRow = WA.CredentialEntry
    <$> (WA.CredentialId             <$> D.column (D.nonNullable D.bytea))
    <*> (WA.UserHandle               <$> D.column (D.nonNullable D.bytea))
    <*> (WA.PublicKeyBytes           <$> D.column (D.nonNullable D.bytea))
    <*> (WA.SignatureCounter . fromIntegral <$> D.column (D.nonNullable D.int4))
    <*> D.column (D.nonNullable transportsValue)

-- bytea column carrying the CBOR-encoded transports list
transportsValue :: D.Value [WA.AuthenticatorTransport]
transportsValue = D.refine decode D.bytea
    where
      decode bytes =
          case deserialiseOrFail (LBS.fromStrict bytes) of
              Left err   -> Left (T.pack ("invalid transports CBOR: " <> show err))
              Right encs -> Right (map WA.decodeAuthenticatorTransport encs)


-- | Inserts a new user into the database. Used during registration.
insertUser ::
    WA.CredentialUserEntity ->
    Session Int64
insertUser user =
    let WA.CredentialUserEntity
            { WA.cueId = WA.UserHandle handle,
              WA.cueName = WA.UserAccountName accountName,
              WA.cueDisplayName = WA.UserAccountDisplayName accountDisplayName
            } = user
    in statement (handle, accountName, accountDisplayName)
        [singletonStatement|
            INSERT INTO users (handle, email, display_name, password) VALUES ($1 :: bytea, $2 :: text, $3 :: text, '') RETURNING id :: int8
        |]

-- | Check if a user exists in the database
userExists :: WA.UserAccountName -> Session Bool
userExists (WA.UserAccountName accountName) = statement accountName stmt
    where
        stmt = dimap Rel.id isJust [maybeStatement|
            SELECT true :: bool FROM users WHERE email = $1 :: text
        |]

insertCredentialEntryStmt :: Statement WA.CredentialEntry Int64
insertCredentialEntryStmt =
    Statement cmd credentialEntryParams (D.singleRow (D.column (D.nonNullable D.int8))) True
  where
    cmd =
        "INSERT INTO credential_entries \
        \  (credential_id, user_handle, public_key, sign_counter, transports, created_at) \
        \VALUES ($1, $2, $3, $4, $5, transaction_timestamp()) \
        \RETURNING id"

-- | Inserts a new credential entry into the database. The example server's
-- logic doesn't allow multiple credential per user, but a typical RP
-- implementation will likely want to support it.
insertCredentialEntry ::
    WA.CredentialEntry ->
    Session Int64
insertCredentialEntry entry = statement entry insertCredentialEntryStmt

-- SELECT ... WHERE credential_id = $1   (0 or 1 row)
queryCredentialEntryByCredentialStmt :: Statement WA.CredentialId (Maybe WA.CredentialEntry)
queryCredentialEntryByCredentialStmt =
    Statement cmd encoder (D.rowMaybe credentialEntryRow) True
  where
    encoder = WA.unCredentialId >$< E.param (E.nonNullable E.bytea)
    cmd =
        "SELECT credential_id, user_handle, public_key, sign_counter, transports \
        \FROM credential_entries WHERE credential_id = $1"

-- | Find a credential entry in the database
queryCredentialEntryByCredential :: WA.CredentialId -> Session (Maybe WA.CredentialEntry)
queryCredentialEntryByCredential cred = statement cred queryCredentialEntryByCredentialStmt

-- many rows -> reuse the same Row decoder with D.rowVector
queryCredentialEntriesByUserStmt :: Statement Text (Vector WA.CredentialEntry)
queryCredentialEntriesByUserStmt =
    Statement cmd (E.param (E.nonNullable E.text)) (D.rowVector credentialEntryRow) True
  where
    cmd =
        "SELECT c.credential_id, c.user_handle, c.public_key, c.sign_counter, c.transports \
        \FROM credential_entries AS c \
        \JOIN users AS u ON u.handle = c.user_handle \
        \WHERE u.email = $1"

-- | Retrieve the credential entries belonging to the specified user. In
-- reality, the logic of the server doesn't actually allow a single user to
-- register multiple credentials.
queryCredentialEntriesByUser :: WA.UserAccountName -> Session (Vector WA.CredentialEntry)
queryCredentialEntriesByUser (WA.UserAccountName accountName) = statement accountName queryCredentialEntriesByUserStmt

-- | Set the new signature counter for the specified credential. Used to check
-- if the authenticator wasn't cloned.
updateSignatureCounter :: WA.CredentialId -> WA.SignatureCounter -> Session Int64
updateSignatureCounter (WA.CredentialId credentialId) (WA.SignatureCounter counter) = statement credentialId stmt
    where
        stmt = [rowsAffectedStatement| UPDATE credential_entries SET sign_counter = sign_counter + 1 WHERE credential_id = $1 :: bytea |]

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
queryUserByAuthToken :: AuthToken -> Session (Maybe WA.UserAccountName)
queryUserByAuthToken (AuthToken token) = statement token stmt
    where
        stmt = dimap Rel.id (fmap WA.UserAccountName) [maybeStatement|
            SELECT account_name :: text FROM auth_tokens JOIN users ON users.handle = auth_tokens.user_handle WHERE token = $1 :: bytea
        |]

-- | Store `AuthToken` to keep the user logged in
insertAuthToken :: AuthToken -> WA.UserHandle -> Session Int64
insertAuthToken (AuthToken token) (WA.UserHandle userHandle) = statement (token, userHandle) stmt
    where
        stmt =[rowsAffectedStatement| INSERT INTO auth_tokens (token, user_handle) VALUES ($1 :: bytea, $2 :: bytea) |]

-- | Remove the `AuthToken` from the database, effectively logging out the user
deleteAuthToken :: AuthToken -> Session Int64
deleteAuthToken (AuthToken token) = statement token stmt
    where
        stmt = [rowsAffectedStatement| DELETE FROM auth_tokens WHERE token = $1 :: bytea |]
