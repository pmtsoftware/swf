{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# HLINT ignore "Redundant id" #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Marker
    ( convert
    , convertAndSave
    , health
    , service
    , pollMarkerJobResult
    , parsePageNoAndOrder
    )
where

import Common

import Control.Lens hiding ((.=))
import Data.Aeson hiding (Options)
import qualified Data.Text as T
import Network.Wreq hiding (header)
import qualified Network.Wreq as Wreq
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Network.HTTP.Client (HttpException(..))
import qualified Web.Scotty.Trans as Scotty
import Web.Scotty.Trans (ScottyT)
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Network.Wai.Parse (FileInfo (..))
import Network.HTTP.Types (status400)
import qualified Data.Map.Strict as Map

service :: ScottyT App ()
service = do
    Scotty.post "/api/marker/start" $ do
        files <- Scotty.files
        case files of
            [file] -> startImport file
            _      -> do
                Scotty.status status400
                Scotty.text "Only one file allowed"
    Scotty.get "/api/marker/job/:id/is-complete" isCompleteHandler
    Scotty.get "/api/marker/job/:id/result" resultPage

data StartResponse = StartResponse
    { ok :: Bool
    , reqId :: Maybe Int64
    } deriving (Generic, Show)
instance ToJSON StartResponse where
    toEncoding = genericToEncoding defaultOptions

startImport :: Scotty.File -> Handler ()
startImport (_, FileInfo{..}) = do
    liftIO $ writeFileBS "urs.pdf" . fromLazy $ fileContent
    orderResult <- liftIO $ orderJob "urs.pdf"
    case orderResult of
        Nothing -> Scotty.json $ StartResponse False Nothing
        Just OrderResult{..} -> do
            result <- queryDb stmt (requestId, requestCheckUrl, "" :: Text)
            case result of
                [Only jid] -> onSuccess jid
                _     -> error ""
            return ()
    where
        stmt = [sql|
            INSERT INTO marker_requests (request_id, request_check_url, status, created_at) VALUES (?, ?, ?, transaction_timestamp()) RETURNING id;
        |]
        onSuccess :: Int64 -> Handler ()
        onSuccess jid = do
            box <- lift $ asks markerRequest
            liftIO $ putMVar box jid
            Scotty.json $ StartResponse True (Just jid)

newtype CompleteStatus = CompleteStatus { completed :: Bool }
    deriving (Generic, Show)
instance ToJSON CompleteStatus where
    toEncoding = genericToEncoding defaultOptions

isCompleteHandler :: Handler ()
isCompleteHandler = do
    paramJobId <- Scotty.captureParam @Int64 "id"
    [Only status] <- queryDb @(Only Int64) @(Only Text) [sql| SELECT status FROM marker_requests WHERE id = ?; |] $ Only paramJobId
    case status of
        "complete" -> Scotty.json $ CompleteStatus True
        _ -> Scotty.json $ CompleteStatus False


resultPage :: Handler ()
resultPage = do
    paramJobId <- Scotty.captureParam @Int64 "id"
    chunks <- queryDb @(Only Int64) @Chunk [sql|
        SELECT blockid, html, block_type, page_no, page_order FROM marker_blocks WHERE request_id = ? ORDER BY page_no, page_order;
    |] $ Only paramJobId
    images <- queryDb @(Only Int64) @Image [sql|
        SELECT name, content FROM marker_images WHERE request_id = ?;
    |] $ Only paramJobId
    Scotty.json $ ChunksWrapper chunks images

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
    , images :: Maybe (Map Text Text)
    } deriving (Generic, Show, Eq)

instance FromJSON JobResult where
    parseJSON = withObject "HealthResponse" $ \v -> JobResult
        <$> v .:? "output_format"
        <*> v .:? "chunks"
        <*> v .: "status"
        <*> v .: "success"
        <*> v .: "page_count"
        <*> v .:? "checkpoint_id"
        <*> v .:? "images"
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

newtype PageOrder = PageOrder Int
    deriving (Show, Eq, Generic)
deriving newtype instance FromField PageOrder
deriving newtype instance ToField PageOrder

newtype BlocksWrapper = BlocksWrapper { blocks :: [Block] }
    deriving (Generic, Show, Eq)

instance FromJSON BlocksWrapper where
    parseJSON = withObject "HealthResponse" $ \v -> BlocksWrapper
        <$> v .: "blocks"
instance ToJSON BlocksWrapper where
    toJSON (BlocksWrapper{..}) =
        object
            [ "blocks" .= blocks
            ]

data Image = Image
    { imgName :: Text
    , imgContent :: Text
    } deriving (Eq, Show, Generic)
instance FromRow Image where
    fromRow = Image <$> field <*> field
instance ToJSON Image where
    toJSON (Image{..}) =
        object
            [ "name" .= imgName
            , "content" .= imgContent
            ]

data ChunksWrapper = ChunksWrapper
    { chunksList :: [Chunk]
    , imagesList :: [Image]
    }
    deriving (Generic, Show, Eq)
instance ToJSON ChunksWrapper where
    toJSON (ChunksWrapper{..}) =
        object
            [ "blocks" .= chunksList
            , "images" .= imagesList
            ]

data Chunk = Chunk
    { chunkId :: Text
    , chunkContent :: Text
    , chunkType :: Text
    , chunkPage :: Int
    , chunkOrder :: Int
    } deriving (Generic, Show, Eq)
instance FromRow Chunk where
    fromRow = Chunk <$> field <*> field <*> field <*> field <*> field
instance ToJSON Chunk where
    toJSON (Chunk{..}) =
        object
            [ "id" .= chunkId
            , "html" .= chunkContent
            , "block_type" .= chunkType
            , "page" .= chunkPage
            , "order" .= chunkOrder
            ]

data Block = Block
    { blockId :: Text
    , html :: Text
    , blockType :: Text
    } deriving (Generic, Show, Eq)
instance FromRow Block where
    fromRow = Block <$> field <*> field <*> field
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

data Prompt = Prompt
    { promptCheckpointId :: Text
    , prompt :: Text
    } deriving (Generic, Show, Eq, ToRow)
instance ToJSON Prompt where
    toJSON (Prompt{..}) =
        object
            [ "checkpoint_id" .= promptCheckpointId
            , "prompt" .= prompt
            ]

data Job = Job
    { jobId :: !Int64
    , jobRequestId :: !Text
    , jobRequestCheckUrl :: !Text
    , jobStatus :: !Text
    , jobCheckpointId :: !(Maybe Text)
    } deriving (Generic, Show, Eq)

instance FromRow Job where
    fromRow = Job <$> field <*> field <*> field <*> field <*> field

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
        run OrderResult{..} = poll 600 (getJobStatus requestCheckUrl) isProcFinished

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
wait = threadDelay 1000000

orderJob :: FilePath -> IO (Maybe OrderResult)
orderJob fp = do
    let payload = [ partFile "file" fp
                  , partLBS "output_format" "chunks"
                  , partLBS "mode" "accurate"
                  -- , partLBS "disable_image_extraction" "true"
                  ]
        url = T.unpack $ baseUrl <> "api/v1/marker"
    r <- asJSON @IO @OrderResult =<< postWith opts url payload
    let result@OrderResult{..} = r ^. responseBody
    if success then return (Just result) else return Nothing

runPrompt :: Text -> Text -> IO (Maybe OrderResult)
runPrompt checkpointId prompt = do
    let payload = toJSON $ Prompt checkpointId prompt
        url = T.unpack $ baseUrl <> "api/v1/marker/prompt"
    r <- asJSON @IO @OrderResult =<< postWith opts url payload
    let result@OrderResult{..} = r ^. responseBody
    if success then return (Just result) else return Nothing

getJobStatus :: Text -> IO JobResult
getJobStatus url = do
    r <- asJSON @IO @JobResult =<< getWith opts (T.unpack url)
    let ps@(JobResult {..}) = r ^. responseBody
    putStrLn . T.unpack $ "Processing status: " <> markerStatus
    whenJust checkpointId print
    return ps

opts :: Options
opts = defaults & appKeyH
    where
        appKeyH = Wreq.header "X-Api-Key" .~ [apiKey]

pollMarkerJobResult :: AppEnv -> IO ()
pollMarkerJobResult AppEnv{..} = forever $ do
    jobId <- takeMVar markerRequest
    threadDelay 1000000
    Job{..} <- getDbRow connPool jobId
    putStrLn " Start polling"
    result <- poll 60 (getJobStatus jobRequestCheckUrl) isProcFinished
    case result of
        Right jr@JobResult{..} -> when (markerStatus == "complete") $ do
            putStrLn "Polling finshed successfully"
            -- writeFileBS "output.json" $ fromLazy (encode jr)
            saveStatusComplete connPool jobId
            whenJust checkpointId $ saveCheckpointId connPool jobId
            storeChunks connPool jobId $ maybe [] blocks chunks
            storeImages connPool jobId $ fromMaybe Map.empty images
            return ()
        Left _ -> putStrLn "Polling failed"
    return ()
    where
        saveCheckpointId :: Pool Connection -> Int64 -> Text -> IO ()
        saveCheckpointId pool jobId checkpointId = withResource pool $ \conn -> do
            _ <- execute conn [sql| UPDATE marker_requests SET checkpoint_id = ? WHERE id = ?; |]  (checkpointId, jobId)
            return ()
        saveStatusComplete :: Pool Connection -> Int64 -> IO ()
        saveStatusComplete pool jobId = withResource pool $ \conn -> do
            _ <- execute conn [sql| UPDATE marker_requests SET status = 'complete' WHERE id = ?; |]  $ Only jobId
            return ()
        getDbRow :: Pool Connection -> Int64 -> IO Job
        getDbRow pool jobId = withResource pool $ \conn -> do
            [entity] <- query conn stmt $ Only jobId
            return entity
        stmt = [sql|
                SELECT id, request_id, request_check_url, status, checkpoint_id FROM marker_requests WHERE id = ?;
            |]

storeChunks :: Pool Connection -> Int64 -> [Block] -> IO ()
storeChunks pool jobId blocks = do
    withResource pool $ \conn -> do
        -- delete previous chunks
        _ <- execute conn [sql| DELETE FROM marker_blocks WHERE request_id = ?; |] $ Only jobId
        _ <- executeMany conn [sql|
            INSERT INTO marker_blocks (request_id, blockid, html, block_type, page_no, page_order) VALUES (?, ?, ?, ?, ?, ?);
        |] $ toRow jobId <$> blocks
        return ()
    where
        toRow :: Int64 -> Block -> (Int64, Text, Text, Text, Maybe Int, PageOrder)
        toRow jid Block{..} = let (pageNo, pageOrder) = parsePageNoAndOrder blockId
            in (jid, blockId, html, blockType, pageNo, fromMaybe defOrder pageOrder)
        defOrder :: PageOrder
        defOrder = PageOrder 0

storeImages :: Pool Connection -> Int64 -> Map Text Text -> IO ()
storeImages pool jobId imgs = do
    withResource pool $ \conn -> do
        -- delete previous chunks
        _ <- execute conn [sql| DELETE FROM marker_images WHERE request_id = ?; |] $ Only jobId
        _ <- executeMany conn [sql|
            INSERT INTO marker_images (request_id, name, content) VALUES (?, ?, ?);
        |] $ toRow jobId <$> Map.toList imgs
        return ()
    where
        toRow :: Int64 -> (Text, Text) -> (Int64, Text, Text)
        toRow jid (k, v) = (jid, k, v)

parsePageNoAndOrder :: Text -> (Maybe Int, Maybe PageOrder)
parsePageNoAndOrder x = bimap (fmap (+1) . convert') (fmap PageOrder . convert') $ getPageAndOrder chunks
    where
        convert' = rightToMaybe . readEither . toString
        chunks = T.split (=='/') x
        getPageAndOrder [_, _, page, _, order] = (page, order)
        getPageAndOrder _ = ("", "")

