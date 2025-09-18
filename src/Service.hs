module Service
( service
) where

import Common

import Web.Scotty.Trans (ScottyT)
import qualified Web.Scotty.Trans as Scotty
import Data.Aeson
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Agent (buildPrompt, go, mapResponse)
import Ollama (GenerateResponse)
import Data.Ollama.Chat (OllamaError)

data Doc = Doc
    { docId :: !Int64
    , docFilename :: Text
    }
    deriving (Generic, Show)
instance ToJSON Doc where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Doc
instance FromRow Doc where
    fromRow = Doc <$> field <*> field

data Response a = Response
    { content :: a
    , ok :: Bool
    }
    deriving (Generic, Show)
instance ToJSON a => ToJSON (Response a) where
    toEncoding = genericToEncoding defaultOptions

data Args = Args
    { argDocId :: Int64
    , argRequirement :: Text
    }
    deriving (Generic, Show)
instance FromJSON Args where
    parseJSON = withObject "Args" $ \v -> Args
        <$> v .: "docId"
        <*> v .: "requirement"

service :: ScottyT App ()
service = do
    Scotty.get "/docs" getDocs
    Scotty.post "/generate" postPrompt

getDocs :: Handler ()
getDocs = do
    docs <- queryDb_ @Doc [sql| SELECT id, file_name FROM docs; |]
    Scotty.json $ Response docs True

postPrompt :: Handler ()
postPrompt = do
    Args{..} <- Scotty.jsonData
    [(fn, content)] <- queryDb [sql| SELECT file_name, md FROM docs WHERE id = ?; |] $ Only argDocId
    let prompt = buildPrompt fn content argRequirement
    result <- liftIO $ go prompt
    either errorResponse okResponse result
    where
        errorResponse :: OllamaError -> Handler ()
        errorResponse oe = Scotty.json $ Response (show @String oe) False

        okResponse :: GenerateResponse -> Handler ()
        okResponse gr = Scotty.json $ Response (mapResponse gr) True

