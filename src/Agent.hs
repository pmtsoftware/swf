{-# LANGUAGE DuplicateRecordFields #-}

module Agent where

import Relude

import Ollama
import Data.Aeson

data AIResponse = AIResponse
    { aiModel :: !Text
    , aiGenResponse :: !Text
    , aiDone :: !Bool
    , aiTotalDuration :: !(Maybe Int64)
    , aiThinking :: !(Maybe Text)
    }
    deriving (Generic, Show)
instance ToJSON AIResponse where
    toEncoding = genericToEncoding defaultOptions

mapResponse :: GenerateResponse -> AIResponse
mapResponse GenerateResponse{..} = AIResponse model genResponse done totalDuration thinking

opts :: Text -> GenerateOps
opts prompt = defaultGenerateOps
    { modelName = "gpt-oss:20b" -- "deepseek-r1"
    , prompt = prompt
    }

buildPrompt :: Text -> Text -> Text -> Text
buildPrompt fn fc requirement
    = "[file name]: " <> fn <> "\n"
    <> "[file content begin]\n"
    <> fc
    <> "[file content end]\n"
    <> "[requirement start]\n"
    <> requirement
    <> "[requirement end]\n"
    <> "File " <> fn <> " in markdown format with some device specification is provided. "
    <> "There is also some requirement in html format. Check if provided specification fullfills provided requirement? "
    <> "Possible answers are - Yes, - No, - Partially. Respond with justification with reference to specification."

go :: Text -> IO (Either OllamaError GenerateResponse)
go prompt = generate (opts prompt) $ Just defaultOllamaConfig { timeout = 1200 }
