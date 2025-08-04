module Marker
    ( convert
    , convertAndSave
    , health
    , service
    )
where

import Common
import Homepage (layoutM)

import Control.Lens hiding ((.=))
import Data.Aeson hiding (Options)
import qualified Data.Text as T
import Network.Wreq
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Network.HTTP.Client (HttpException(..))
import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT)
import Text.Blaze.Html5 hiding (header, object)
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text

service :: ScottyT App ()
service = do
    Scotty.get "/marker/start" importForm
    Scotty.post "/marker/start" undefined
    Scotty.get "/marker/job/:id/status" undefined
    Scotty.get "/marker/job/:id/result" undefined
    Scotty.post "/marker/job/:id/refine" undefined

importForm :: Handler ()
importForm = do
    layout <- layoutM
    Scotty.html . renderHtml $ layout $ do
        h1 "Import pdf"
        form ! method "POST" ! enctype "multipart/form-data" $ do
            label $ do
                "File"
                input ! required "required" ! name "file" ! type_ "file" ! accept ".pdf"
            button ! type_ "submit" $ "Start"

type Converter = FilePath -> IO (Either () JobResult)

newtype HealthResponse = HealthResponse
    { status :: Text
    } deriving (Generic, Show, Eq)

instance FromJSON HealthResponse where
    parseJSON = withObject "HealthResponse" $ \v -> HealthResponse
        <$> v .: "status"

data OrderResult = OrderResult
    { success :: Bool
    , apiError :: Maybe Text
    , requestId :: Text
    , requestCheckUrl :: Text
    } deriving (Generic, Show, Eq)

instance FromJSON OrderResult where
    parseJSON = withObject "HealthResponse" $ \v -> OrderResult
        <$> v .: "success"
        <*> v .:? "error"
        <*> v .: "request_id"
        <*> v .: "request_check_url"

data JobResult = JobResult
    { outputFormat :: Maybe Text
    , chunks :: Maybe BlocksWrapper
    , markerStatus :: Text
    , markerSuccess :: Maybe Bool
    , pageCount :: Maybe Int
    , checkpointId :: Maybe Text
    } deriving (Generic, Show, Eq)

instance FromJSON JobResult where
    parseJSON = withObject "HealthResponse" $ \v -> JobResult
        <$> v .:? "output_format"
        <*> v .:? "chunks"
        <*> v .: "status"
        <*> v .: "success"
        <*> v .: "page_count"
        <*> v .:? "checkpoint_id"
instance ToJSON JobResult where
    toJSON (JobResult{..}) =
        object
            [ "output_format" .= outputFormat
            , "chunks" .= chunks
            , "status" .= markerStatus
            , "success" .= markerSuccess
            , "page_count" .= pageCount
            , "checkpoint_id" .= checkpointId
            ]

data BlocksWrapper = BlocksWrapper { blocks :: [Block] }
    deriving (Generic, Show, Eq)

instance FromJSON BlocksWrapper where
    parseJSON = withObject "HealthResponse" $ \v -> BlocksWrapper
        <$> v .: "blocks"
instance ToJSON BlocksWrapper where
    toJSON (BlocksWrapper{..}) =
        object
            [ "blocks" .= blocks
            ]

data Block = Block
    { blockId :: Text
    , html :: Text
    , blockType :: Text
    } deriving (Generic, Show, Eq)

instance FromJSON Block where
    parseJSON = withObject "HealthResponse" $ \v -> Block
        <$> v .: "id"
        <*> v .: "html"
        <*> v .: "block_type"
instance ToJSON Block where
    toJSON (Block{..}) =
        object
            [ "id" .= blockId
            , "html" .= html
            , "block_type" .= blockType
            ]

data Job = Job
    { jobId :: !Int64
    , jobRequestId :: !Text
    , jobRequestCheckUrl :: !Text
    , jobStatus :: !Text
    , jobCheckpointId :: !(Maybe Text)
    } deriving (Generic, Show, Eq)

apiKey :: ByteString
apiKey = "Gxj_Gk8AKREH-oBHi3jvRP4P_yE6nzDxcgqZ-0uUqNY"

baseUrl :: Text
baseUrl = "https://www.datalab.to/"

health :: IO Text
health = do
    r <- asJSON @IO @HealthResponse =<< getWith opts url
    let HealthResponse{..} =  r ^. responseBody
    return status
    where
        url = T.unpack $ baseUrl <> "api/v1/user_health"

convert :: Converter
convert fp = do
    orderResult <- orderJob fp
    -- wait
    threadDelay 2000000
    result <- traverse run orderResult
    case result of
        Just (Right jr@JobResult{..}) -> return $ Right jr
        _ -> return $ Left ()
    where
        run :: OrderResult -> IO (Either Text JobResult)
        run OrderResult{..} = poll 60 (getJobStatus requestCheckUrl) isProcFinished
        isProcFinished :: JobResult -> Bool
        isProcFinished JobResult{..} = markerStatus == "complete"

convertAndSave :: FilePath -> IO ()
convertAndSave fp = do
    result <- convert fp
    case result of
        Right bs -> writeFileBS "doc.json" $ toStrict . encode $ bs
        Left _ -> return ()

poll :: Int -> IO a -> (a -> Bool) -> IO (Either Text a)
poll attempts ask' isFinished
    | attempts == 0 = return $ Left "[ERROR] Timeout"
    | otherwise = do
        result <- wait >> try ask'
        case result of
            Left ex -> do
                handleException ex
                poll (attempts - 1) ask' isFinished
            Right result' -> if isFinished result' then return $ Right result' else poll (attempts - 1) ask' isFinished

    where
        handleException :: HttpException -> IO ()
        handleException = print

wait :: IO ()
wait = threadDelay 2000000

orderJob :: FilePath -> IO (Maybe OrderResult)
orderJob fp = do
    let payload = [ partFile "file" fp
                  , partLBS "output_format" "chunks"
                  , partLBS "use_llm" "false"
                  , partLBS "save_checkpoint" "true"
                  , partLBS "disable_image_extraction" "true"
                  ]
        url = T.unpack $ baseUrl <> "api/v1/marker"
    r <- asJSON @IO @OrderResult =<< postWith opts url payload
    let result@OrderResult{..} = r ^. responseBody
    -- putStrLn "Marker API call result"
    -- putStr "Success: "
    -- print success
    -- putStr "Request id: "
    -- print requestId
    -- putStr "Request check url: "
    -- print requestCheckUrl
    -- putStrLn  "Checkpoint id: "
    -- print checkpointId
    if success then return (Just result) else return Nothing

getJobStatus :: Text -> IO JobResult
getJobStatus url = do
    r <- asJSON @IO @JobResult =<< getWith opts (T.unpack url)
    let ps@(JobResult {..}) = r ^. responseBody
    putStrLn . T.unpack $ "Processing status: " <> markerStatus
    return ps

opts :: Options
opts = defaults & appKeyH
    where
        appKeyH = header "X-Api-Key" .~ [apiKey]
