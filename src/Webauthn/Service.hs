{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Webauthn.Service (service) where

import Common

import Web.Scotty.Trans (ScottyT)
import qualified Web.Scotty.Trans as Scotty
import qualified Network.HTTP.Types as HTTP
import qualified Crypto.WebAuthn as WA
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Text.IO as TIO
import Data.Aeson (FromJSON, ToJSON, Value (String))
import Webauthn.Database (userExists, queryCredentialEntryByCredential, insertUser, insertCredentialEntry, queryCredentialEntriesByUser, updateSignatureCounter)
import Webauthn.PendingCeremonies (insertPendingRegistration, getPendingRegistration, insertPendingAuthentication, getPendingAuthentication)
import Data.Validation (Validation (Failure, Success))
import System.Hourglass (dateCurrent)

data RegisterBeginReq = RegisterBeginReq
    { accountName :: Text
    , accountDisplayName :: Text
    }
    deriving (Show, FromJSON, ToJSON)
    deriving stock (Generic)

service :: ScottyT App ()
service = do
    Scotty.post "/webauthn/register/begin" beginRegistration
    Scotty.post "/webauthn/register/complete" completeRegistration
    Scotty.post "/webauthn/login/begin" beginLogin
    Scotty.post "/webauthn/login/complete" completeLogin

-- | Utility function for debugging. Creates a human-readable bytestring from
-- any value that can be encoded to JSON. We use this function to provide a log
-- of all messages received and sent.
jsonText :: (ToJSON a) => a -> Text
jsonText = decodeUtf8 . toStrict . AP.encodePretty' config
    where
        config :: AP.Config
        config = AP.defConfig
            { AP.confIndent = AP.Spaces 2
            , AP.confCompare = AP.compare
            , AP.confNumFormat = AP.Decimal
            }

-- | In this function we receive the intent of the client to register and reply
-- with the
-- [creation options](https://www.w3.org/TR/webauthn-2/#dictionary-makecredentialoptions)
-- . This function also checks if the specified user hasn't already registered.
-- WebAuthn does allow a single user to register multiple credentials, but this
-- server doesn't implement it.
beginRegistration :: Handler ()
beginRegistration = do
    req@RegisterBeginReq {accountName, accountDisplayName} <- Scotty.jsonData @RegisterBeginReq
    Scotty.liftAndCatchIO $ TIO.putStrLn $ "Register begin <= " <> jsonText req
    AppEnv{..} <- lift ask
    exists <- Scotty.liftAndCatchIO $
        withResource connPool $ \conn -> userExists conn (WA.UserAccountName accountName)
    when exists $ Scotty.raiseStatus HTTP.status409 "Account name already taken"
    userId <- Scotty.liftAndCatchIO WA.generateUserHandle
    let user = WA.CredentialUserEntity
                { WA.cueId = userId
                , WA.cueDisplayName = WA.UserAccountDisplayName accountDisplayName
                , WA.cueName = WA.UserAccountName accountName
                }
    options <- Scotty.liftAndCatchIO $ insertPendingRegistration pendingCeremonies $ defaultPkcco user
    Scotty.liftAndCatchIO $ TIO.putStrLn $ "Register begin => " <> jsonText options
    Scotty.json $ WA.wjEncodeCredentialOptionsRegistration options

-- | The default
-- [creation options](https://www.w3.org/TR/webauthn-2/#dictionary-makecredentialoptions).
-- For simplicity's sake this server stores the entirety of the options in the
-- `PendingCeremonies`. However, only a subset of these options are used by the
-- `verify` functions. See the `WA.CredentialOptions` documentation for more
-- information.
defaultPkcco :: WA.CredentialUserEntity -> WA.Challenge -> WA.CredentialOptions 'WA.Registration
defaultPkcco userEntity challenge =
    WA.CredentialOptionsRegistration
    { WA.corRp = WA.CredentialRpEntity {WA.creId = Nothing, WA.creName = "ACME"}
    , WA.corUser = userEntity
    , WA.corChallenge = challenge
    , WA.corPubKeyCredParams =
        [ WA.CredentialParameters
            { WA.cpTyp = WA.CredentialTypePublicKey
            , WA.cpAlg = WA.CoseAlgorithmES256
            }
        , WA.CredentialParameters
            { WA.cpTyp = WA.CredentialTypePublicKey
            , WA.cpAlg = WA.CoseAlgorithmRS256
            }
        ]
    , WA.corTimeout = Nothing
    , WA.corExcludeCredentials = []
    , WA.corAuthenticatorSelection =
        Just
            WA.AuthenticatorSelectionCriteria
            { WA.ascAuthenticatorAttachment = Nothing
            , WA.ascResidentKey = WA.ResidentKeyRequirementDiscouraged
            , WA.ascUserVerification = WA.UserVerificationRequirementPreferred
            }
    , WA.corAttestation = WA.AttestationConveyancePreferenceDirect
    , WA.corExtensions = Nothing
    }

origin :: WA.Origin
origin = "http://localhost:3001"

-- | Completes the relying party's responsibilities of the registration
-- ceremony. Receives the credential from the client and performs the
-- [registration operation](https://www.w3.org/TR/webauthn-2/#sctn-registering-a-new-credential).
-- If the operation succeeds, the user is added to the database, logged in, and
-- redirected to the @authenticated.html@ page.
completeRegistration :: Handler ()
completeRegistration = do
    credential <- Scotty.jsonData
    Scotty.liftAndCatchIO $ TIO.putStrLn $ "Raw register complete <= " <> jsonText credential
    cred <- case WA.wjDecodeCredentialRegistration credential of
        Left err -> do
            Scotty.liftAndCatchIO $ TIO.putStrLn $ "Register complete failed to decode raw request: " <> err
            fail $ show err
        Right result -> pure result
    Scotty.liftAndCatchIO $ TIO.putStrLn $ "Register complete <= " <> jsonText (WA.stripRawCredential cred)

    AppEnv {..} <- lift ask
    options <- Scotty.liftAndCatchIO (getPendingRegistration pendingCeremonies cred) >>= \case
        Left err -> do
            Scotty.liftAndCatchIO $ TIO.putStrLn $ "Register complete problem with challenge: " <> jsonText (String $ toText err)
            Scotty.raiseStatus HTTP.status401 $ "Challenge error: " <> toLText err
        Right result -> pure result

    let userHandle = WA.cueId $ WA.corUser options

    registry' <- Scotty.liftAndCatchIO $ readTVarIO registry
    now <- Scotty.liftAndCatchIO dateCurrent
    result <- case WA.verifyRegistrationResponse (one origin) rpIdHash registry' now options cred of
        Failure errs@(err :| _) -> do
            Scotty.liftAndCatchIO $ TIO.putStrLn $ "Register complete had errors: " <> show @Text errs
            fail $ show err
        Success result -> pure result

    Scotty.liftAndCatchIO $ TIO.putStrLn $ "Register complete result: " <> jsonText result
    -- if the credential was succesfully attested, we will see if the
    -- credential doesn't exist yet, and if it doesn't, insert it.
    Scotty.liftAndCatchIO $
        withResource connPool $ \conn -> do
            -- If a credential with this id existed already, it must belong to the
            -- current user, otherwise it's an error. The spec allows removing the
            -- credential from the old user instead, but we don't do that.
            mexistingEntry <- queryCredentialEntryByCredential conn (WA.ceCredentialId $ WA.rrEntry result)
            case mexistingEntry of
                Nothing -> do
                    _ <- insertUser conn $ WA.corUser options
                    _ <- insertCredentialEntry conn $ WA.rrEntry result
                    pure ()
                Just existingEntry | userHandle == WA.ceUserHandle existingEntry -> pure ()
                Just differentEntry -> do
                    TIO.putStrLn $ "Register complete credential already belongs to the user credential entry: " <> jsonText differentEntry
                    fail "This credential is already registered"
    -- TODO: authenticated
    let response = String "success"
    Scotty.liftAndCatchIO $ TIO.putStrLn $ "Register complete => " <> jsonText response
    Scotty.json response

beginLogin :: Handler ()
beginLogin = do
    accountName <- WA.UserAccountName <$> Scotty.jsonData @Text
    Scotty.liftAndCatchIO $ TIO.putStrLn $ "Login begin <= " <> jsonText accountName

    AppEnv{..} <- lift ask
    credentials <- Scotty.liftAndCatchIO $
        withResource connPool $ \conn -> do
            queryCredentialEntriesByUser conn accountName
    when (null credentials) $ do
        Scotty.liftAndCatchIO $ TIO.putStrLn "Login begin error: User not found"
        Scotty.raiseStatus HTTP.status404 "User not found"

    -- Create credential options from the credential retrieved from the database
    -- and insert the options into the pending ceremonies. This server stores the
    -- entire options, but this isn't actually necessary a fully spec complient
    -- RP implementation. See the documentation of `WA.CredentialOptions` for
    -- more information.
    options <- Scotty.liftAndCatchIO $
        insertPendingAuthentication pendingCeremonies $ \challenge -> do
            WA.CredentialOptionsAuthentication
                { WA.coaRpId = Nothing,
                    WA.coaTimeout = Nothing,
                    WA.coaChallenge = challenge,
                    WA.coaAllowCredentials = fmap mkCredentialDescriptor credentials,
                    WA.coaUserVerification = WA.UserVerificationRequirementPreferred,
                    WA.coaExtensions = Nothing
                }
 
    -- Send credential options to the client
    Scotty.liftAndCatchIO $ TIO.putStrLn $ "Login begin => " <> jsonText options
    Scotty.json $ WA.wjEncodeCredentialOptionsAuthentication options
    where
        mkCredentialDescriptor :: WA.CredentialEntry -> WA.CredentialDescriptor
        mkCredentialDescriptor WA.CredentialEntry {WA.ceCredentialId, WA.ceTransports} =
            WA.CredentialDescriptor
                { WA.cdTyp = WA.CredentialTypePublicKey
                , WA.cdId = ceCredentialId
                , WA.cdTransports = Just ceTransports
                }

-- | Completes the relying party's responsibilities of the authentication
-- ceremony. Receives the credential from the client and performs the
-- [authentication operation](https://www.w3.org/TR/webauthn-2/#sctn-verifying-assertion).
-- If the operation succeeds, the user is logged in, and
-- redirected to the @authenticated.html@ page.
completeLogin :: Handler ()
completeLogin = do
    -- Receive the credential from the client
    credential <- Scotty.jsonData
    Scotty.liftAndCatchIO $ TIO.putStrLn $ "Raw login complete <= " <> jsonText credential

    -- Decode credential
    cred <- case WA.wjDecodeCredentialAuthentication credential of
        Left err -> do
            Scotty.liftAndCatchIO $ TIO.putStrLn $ "Login complete failed to decode request: " <> show err
            fail $ show err
        Right result -> pure result
    Scotty.liftAndCatchIO $ TIO.putStrLn $ "Login complete <= " <> jsonText (WA.stripRawCredential cred)

    AppEnv{..} <- lift ask
    -- Retrieve stored options from the pendingOptions
    options <-
        Scotty.liftAndCatchIO (getPendingAuthentication pendingCeremonies cred) >>= \case
            Left err -> do
                Scotty.liftAndCatchIO $ TIO.putStrLn $ "Login complete problem with challenge: " <> jsonText (String $ show err)
                Scotty.raiseStatus HTTP.status401 $ "Challenge error: " <> show err
            Right result -> pure result

    -- Check database for user, abort if user is unknown.
    mentry <- Scotty.liftAndCatchIO $
        withResource connPool $ \conn ->
            queryCredentialEntryByCredential conn (WA.cIdentifier cred)
    entry <- case mentry of
        Nothing -> do
            Scotty.liftAndCatchIO $ TIO.putStrLn "Login complete credential entry doesn't exist"
            fail "Credential not found"
        Just entry -> pure entry

    -- Perform the verification of the credential. Abort if the credential could
    -- not be verified.
    let verificationResult =
            WA.verifyAuthenticationResponse
                (one origin)
                rpIdHash
                (Just (WA.ceUserHandle entry))
                entry
                options
                cred
    WA.AuthenticationResult newSigCount <- case verificationResult of
        Failure errs@(err :| _) -> do
            Scotty.liftAndCatchIO $ TIO.putStrLn $ "Login complete had errors: " <> show errs
            fail $ show err
        Success result -> pure result

    -- Update signature counter in the database/abort if it was potentially cloned
    case newSigCount of
        WA.SignatureCounterZero ->
            Scotty.liftAndCatchIO $
            TIO.putStrLn "SignatureCounter is Zero"
        (WA.SignatureCounterUpdated counter) ->
            Scotty.liftAndCatchIO $ do
            TIO.putStrLn $ "Updating SignatureCounter to: " <> show counter
            withResource connPool $
                \conn -> do
                    _ <- updateSignatureCounter conn (WA.cIdentifier cred) counter
                    return ()
        WA.SignatureCounterPotentiallyCloned -> Scotty.raiseStatus HTTP.status401 "Signature Counter Cloned"

    -- Set the login cookie and send the result to the server
    -- setAuthenticatedAs db (WA.ceUserHandle entry)
    let result = String "success"
    Scotty.liftAndCatchIO $ TIO.putStrLn $ "Login complete => " <> jsonText result
    Scotty.json result
