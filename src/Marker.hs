{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}

module Marker
    ( convert
    , convertAndSave
    , health
    , service
    , pollMarkerJobResult
    , parsePageNo
    )
where

import Common
import Homepage (layoutM)

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
import Text.Blaze.Html5 hiding (object)
import Text.Blaze.Html5.Attributes hiding (title, form, label)
import Text.Blaze.Html.Renderer.Text
import Database.PostgreSQL.Simple.FromRow (fromRow, field)
import Network.Wai.Parse (FileInfo (..))
import qualified Data.List.NonEmpty as NE
import TextShow hiding (toString)

service :: ScottyT App ()
service = do
    Scotty.get "/marker/start" importForm
    Scotty.post "/marker/start" $ do
        files <- Scotty.files
        case files of
            [file] -> startImport file
            _      -> Scotty.redirect "/marker/error"

    Scotty.get "/marker/job/:id/status" statusPage
    Scotty.get "/marker/job/:id/is-complete" isCompleteHandler
    Scotty.get "/marker/job/:id/result" resultPage
    Scotty.post "/marker/refine" statusPage
    Scotty.get "/marker/error" errorPage

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

startImport :: Scotty.File -> Handler ()
startImport (_, FileInfo{..}) = do
    liftIO $ writeFileBS "urs.pdf" . fromLazy $ fileContent
    orderResult <- liftIO $ orderJob "urs.pdf"
    case orderResult of
        Nothing -> Scotty.redirect "/marker/error"
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
            Scotty.redirect $ "/marker/job/" <> showtl jid <> "/status"

promptHandler :: Handler ()
promptHandler = do
    checkpointId <- Scotty.formParam @Text "checkpoint_id"
    prompt <- Scotty.formParam @Text "prompt"
    jobId <- Scotty.formParam @Int64 "id"
    r <- liftIO $ runPrompt checkpointId prompt
    case r of
        Nothing -> Scotty.redirect "/marker/error"
        Just OrderResult{..} -> do
            _ <- executeDb [sql| UPDATE marker_requests SET request_check_url = ?, status = '' WHERE id = ?; |] (requestCheckUrl, jobId)
            box <- lift $ asks markerRequest
            liftIO $ putMVar box jobId

statusPage :: Handler ()
statusPage = do
    paramJobId <- Scotty.captureParam @Int64 "id"
    layout <- layoutM
    Scotty.html . renderHtml $ layout $ do
        h1 "Processing"
        div ! hx_get paramJobId ! hx_trigger ! hx_swap $ do
            p "Please wait..."
    where
        hx_get jobId = customAttribute "hx-get" $ "/marker/job/" <> toValue (showtl jobId) <> "/is-complete"
        hx_trigger = customAttribute "hx-trigger" "load delay:1s"
        hx_swap = customAttribute "hx-swap" "outerHTML"

isCompleteHandler :: Handler ()
isCompleteHandler = do
    paramJobId <- Scotty.captureParam @Int64 "id"
    [Only status] <- queryDb @(Only Int64) @(Only Text) [sql| SELECT status FROM marker_requests WHERE id = ?; |] $ Only paramJobId
    case status of
        "complete" -> Scotty.html. renderHtml $ do
            p "Processing completed! "
            a ! href ("/marker/job/" <> toValue (showtl paramJobId) <> "/result") $ "Open document"
        _ -> Scotty.html . renderHtml $ do
            div ! hx_get paramJobId ! hx_trigger ! hx_swap $ do
                p "Please wait..."
    where
        hx_get jobId = customAttribute "hx-get" $ "/marker/job/" <> toValue (showtl jobId) <> "/is-complete"
        hx_trigger = customAttribute "hx-trigger" "load delay:1s"
        hx_swap = customAttribute "hx-swap" "outerHTML"

resultPage :: Handler ()
resultPage = do
    paramJobId <- Scotty.captureParam @Int64 "id"
    layout <- layoutM
    [Only checkpointId] <- queryDb @(Only Int64) @(Only Text) [sql| SELECT checkpoint_id FROM marker_requests WHERE id = ?; |] $ Only paramJobId
    result <- queryDb [sql|
        SELECT html, page_no FROM marker_blocks WHERE request_id = ? AND page_no IS NOT NULL ORDER BY page_no;
    |] (Only paramJobId)
    Scotty.html . renderHtml $ layout $ do
        div ! class_ "wrapper" $ do
            div ! id "prompting"  $ promptForm paramJobId checkpointId
            div ! id "document" $ getContent result
            div $ do
                code ! id "logs" $ do
                    p "Welcome 🙋"
    where
        getContent :: [(Text, Int)] -> Html
        getContent = renderPages . NE.groupWith snd

renderPages :: [NonEmpty (Text, Int)] -> Html
renderPages = mconcat . fmap renderPage

renderPage :: NonEmpty (Text, Int) -> Html
renderPage sections = article $ do
    let pageNo = snd . NE.head $ sections
    -- header $ small $ text ("Page " <> showt pageNo)
    mapM_ preEscapedToHtml $ fmap fst sections
    footer $ small $  text ("Page " <> showt pageNo)

promptForm :: Int64 -> Text -> Html
promptForm jobId checkpointId = form ! method "POST" ! action "/marker/refine" $ do
    input ! type_ "hidden" ! name "checkpoint_id" ! value (toValue checkpointId)
    input ! type_ "hidden" ! name "id" ! value (toValue jobId)
    label $ do
        "Prompt"
        textarea ! name "prompt" $ text ""
    button ! type_ "submit" $ "Post"

errorPage :: Handler ()
errorPage = do
    layout <- layoutM
    Scotty.html . renderHtml $ layout $ do
        h1 "Error occured!"

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
    } deriving (Generic, Show, Eq, ToRow)

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
wait = threadDelay 1000000

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
        Right JobResult{..} -> when (markerStatus == "complete") $ do
            putStrLn "Polling finshed successfully"
            saveCheckpointId connPool jobId $ fromMaybe "" checkpointId
            -- TODO: insert all chunks
            storeChunks connPool jobId $ maybe [] blocks chunks
            return ()
        Left _ -> putStrLn "Polling failed"
    return ()
    where
        saveCheckpointId :: Pool Connection -> Int64 -> Text -> IO ()
        saveCheckpointId pool jobId checkpointId = withResource pool $ \conn -> do
            _ <- execute conn [sql| UPDATE marker_requests SET checkpoint_id = ?, status = 'complete' WHERE id = ?; |]  (checkpointId, jobId)
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
            INSERT INTO marker_blocks (request_id, blockid, html, block_type, page_no) VALUES (?, ?, ?, ?, ?);
        |] $ toRow jobId <$> blocks
        return ()
    where
        toRow :: Int64 -> Block -> (Int64, Text, Text, Text, Maybe Int)
        toRow jid Block{..} = (jid, blockId, html, blockType, parsePageNo blockId)

parsePageNo :: Text -> Maybe Int
parsePageNo x = rightToMaybe . readEither . toString $ getPage chunks
    where
        chunks = T.split (=='/') x
        getPage [_, _, page, _, _] = page
        getPage _ = ""
