{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Marker
    ( health
    , service
    , pollMarkerJobResult
    , parsePageNoAndOrder
    )
where

import Common

import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Req
import Network.HTTP.Client.MultipartFormData (partFileRequestBody, partBS)
import Network.HTTP.Client (RequestBody (RequestBodyLBS))
import Text.URI (mkURI)
import Control.Concurrent (threadDelay, writeChan, readChan)
import Control.Exception (try)
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

fileTypeFor :: ByteString -> Text
fileTypeFor "application/pdf" = "pdf"
fileTypeFor "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = "excel"
fileTypeFor "application/vnd.ms-excel" = "excel"
fileTypeFor "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "word"
fileTypeFor "application/msword" = "word"
fileTypeFor _ = "unknown"

allowedContentTypes :: [ByteString]
allowedContentTypes =
    [ "application/pdf"
    , "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    , "application/vnd.ms-excel"
    , "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    , "application/msword"
    ]

startImport :: Scotty.File -> Handler ()
startImport (_, FileInfo{..}) = do
    when (fileContentType `notElem` allowedContentTypes) $ do
        Scotty.status status400
        Scotty.text "Unsupported file type. Only PDF and Excel files are allowed."
        Scotty.finish
    orderResult <- liftIO $ orderJob fileContent (toString (decodeUtf8 @Text fileName))
    case orderResult of
        Nothing -> Scotty.json $ StartResponse False Nothing
        Just OrderResult{..} -> do
            result <- queryDb stmt (requestId, requestCheckUrl, "" :: Text, fileTypeFor fileContentType)
            case result of
                [Only jid] -> onSuccess jid
                _     -> error ""
            return ()
    where
        stmt = [sql|
            INSERT INTO marker_requests (request_id, request_check_url, status, file_type, created_at) VALUES (?, ?, ?, ?, transaction_timestamp()) RETURNING id;
        |]
        onSuccess :: Int64 -> Handler ()
        onSuccess jid = do
            fifo <- lift $ asks markerFifo
            liftIO $ writeChan fifo jid
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

data JsonPage = JsonPage { jpChildren :: [Block] }
    deriving (Generic, Show, Eq)
instance FromJSON JsonPage where
    parseJSON = withObject "JsonPage" $ \v -> JsonPage <$> v .: "children"

newtype JsonOutput = JsonOutput { joChildren :: [JsonPage] }
    deriving (Generic, Show, Eq)
instance FromJSON JsonOutput where
    parseJSON = withObject "JsonOutput" $ \v -> JsonOutput <$> v .: "children"

data JobResult = JobResult
    { outputFormat :: Maybe Text
    , chunks :: Maybe BlocksWrapper
    , jsonOutput :: Maybe JsonOutput
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
        <*> v .:? "json"
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
    , blockPage :: Maybe Int
    } deriving (Generic, Show, Eq)
instance FromRow Block where
    fromRow = Block <$> field <*> field <*> field <*> pure Nothing
instance FromJSON Block where
    parseJSON = withObject "HealthResponse" $ \v -> Block
        <$> v .:  "id"
        <*> v .:  "html"
        <*> v .:  "block_type"
        <*> v .:? "page"
instance ToJSON Block where
    toJSON (Block{..}) =
        object
            [ "id"         .= blockId
            , "html"       .= html
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

markerOpts :: Option scheme
markerOpts = header "X-Api-Key" apiKey

health :: IO Text
health = do
    r <- runReq defaultHttpConfig $
        req GET (https "www.datalab.to" /: "api" /: "v1" /: "user_health") NoReqBody jsonResponse markerOpts
    let HealthResponse{..} = responseBody r
    return status

isProcFinished :: (JobResult, LByteString) -> Bool
isProcFinished (JobResult{..}, _) = markerStatus == "complete"

poll :: IO (Either b a) -> (a -> Bool) -> IO (Either b a)
poll ask' isFinished = do
    result <- wait >> ask'
    case result of
        Left _ -> pure result
        Right result' -> if isFinished result'
            then return $ Right result'
            else poll ask' isFinished

wait :: IO ()
wait = threadDelay 1000000

orderJob :: LByteString -> FilePath -> IO (Maybe OrderResult)
orderJob content filename = do
    putTextLn "Start ordering..."
    let parts = [ partFileRequestBody "file" filename (RequestBodyLBS content)
                , partBS "output_format" "chunks"
                , partBS "mode" "accurate"
                ]
    body <- reqBodyMultipart parts
    r <- runReq defaultHttpConfig $
        req POST (https "www.datalab.to" /: "api" /: "v1" /: "marker") body jsonResponse markerOpts
    let result@OrderResult{..} = responseBody r
    if success then return (Just result) else return Nothing

data CheckStatusFailure
    = HttpsUriSchemeExpected
    | HttpError
    | JsonDecodeError
    deriving (Show, Eq, Generic)

getJobStatus :: Text -> IO (Either CheckStatusFailure (JobResult, LByteString))
getJobStatus checkUrl = do
    uri <- mkURI checkUrl
    maybe httpsUriSchemeExpected getJobStatus' $ useHttpsURI uri
    where
    httpsUriSchemeExpected = pure $ Left HttpsUriSchemeExpected
    getJobStatus' (url, urlOpts) = do
        resultOrFailure <- try $ runReq defaultHttpConfig $
            req GET url NoReqBody lbsResponse (markerOpts <> urlOpts)
        pure $ either handleFailure handleSuccess resultOrFailure

    handleFailure :: HttpException -> Either CheckStatusFailure (JobResult, LByteString)
    handleFailure _ = Left HttpError
    handleSuccess r = let body = responseBody r
        in case eitherDecode body of
            Left _ -> Left JsonDecodeError
            Right ps -> Right (ps, body)

pollMarkerJobResult :: AppEnv -> IO ()
pollMarkerJobResult AppEnv{..} = forever $ do
    jobId <- readChan markerFifo
    threadDelay 1000000
    Job{..} <- getDbRow connPool jobId
    putStrLn " Start polling"
    result <- poll (getJobStatus jobRequestCheckUrl) isProcFinished
    case result of
        Right (JobResult{..}, rawBody) -> when (markerStatus == "complete") $ do
            putStrLn "Polling finshed successfully"
            saveStatusComplete connPool jobId True
            whenJust checkpointId $ saveCheckpointId connPool jobId
            saveResponse connPool jobId rawBody
            let chunksBlocks = maybe [] blocks chunks
                jsonBlocks   = maybe [] (concatMap jpChildren . joChildren) jsonOutput
                allBlocks    = if null jsonBlocks then chunksBlocks else jsonBlocks
            storeChunks connPool jobId allBlocks
            storeImages connPool jobId $ fromMaybe Map.empty images
            return ()
        Left _ -> saveStatusComplete connPool jobId False
    return ()
    where
        saveResponse :: Pool Connection -> Int64 -> LByteString -> IO ()
        saveResponse pool jobId body = withResource pool $ \conn -> do
            let val = decode @Value body
            _ <- execute conn [sql| UPDATE marker_requests SET response = ? WHERE id = ?; |] (val, jobId)
            return ()
        saveCheckpointId :: Pool Connection -> Int64 -> Text -> IO ()
        saveCheckpointId pool jobId checkpointId = withResource pool $ \conn -> do
            _ <- execute conn [sql| UPDATE marker_requests SET checkpoint_id = ? WHERE id = ?; |]  (checkpointId, jobId)
            return ()
        saveStatusComplete :: Pool Connection -> Int64 -> Bool -> IO ()
        saveStatusComplete pool jobId success = withResource pool $ \conn -> do
            _ <- execute conn [sql|
                UPDATE marker_requests SET status = 'complete', success = ? WHERE id = ?;
            |] (success, jobId)
            return ()
        getDbRow :: Pool Connection -> Int64 -> IO Job
        getDbRow pool jobId = withResource pool $ \conn -> do
            [entity] <- query conn stmt $ Only jobId
            return entity
        stmt = [sql|
                SELECT id, request_id, request_check_url, status, checkpoint_id FROM marker_requests WHERE id = ?;
            |]

storeChunks :: Pool Connection -> Int64 -> [Block] -> IO ()
storeChunks pool jobId blocks = withResource pool $ \conn -> do
    _ <- execute conn [sql| DELETE FROM marker_blocks WHERE request_id = ?; |] $ Only jobId
    unless (null blocks) $
        void $ executeMany conn [sql|
            INSERT INTO marker_blocks (request_id, blockid, html, block_type, page_no, page_order) VALUES (?, ?, ?, ?, ?, ?);
        |] $ toRow jobId <$> blocks
    where
        toRow :: Int64 -> Block -> (Int64, Text, Text, Text, Maybe Int, PageOrder)
        toRow jid Block{..} =
            let (pageNo, pageOrder) = parsePageNoAndOrder blockId
                effectivePage = pageNo <|> fmap (+1) blockPage
            in (jid, blockId, html, blockType, effectivePage, fromMaybe defOrder pageOrder)
        defOrder :: PageOrder
        defOrder = PageOrder 0

storeImages :: Pool Connection -> Int64 -> Map Text Text -> IO ()
storeImages pool jobId imgs = withResource pool $ \conn -> do
    _ <- execute conn [sql| DELETE FROM marker_images WHERE request_id = ?; |] $ Only jobId
    unless (Map.null imgs) $
        void $ executeMany conn [sql|
            INSERT INTO marker_images (request_id, name, content) VALUES (?, ?, ?);
        |] $ toRow jobId <$> Map.toList imgs
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

