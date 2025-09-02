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
import Text.Blaze.Html5.Attributes hiding (title, form, label, span)
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
    Scotty.post "/marker/page/prev" prevPageHandler
    Scotty.post "/marker/page/next" nextPageHandler
    Scotty.post "/marker/refine" promptHandler
    Scotty.get "/marker/is-refined" isRefinedHandler
    Scotty.put "/marker/template" saveTemplateHandler
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
    jobId <- Scotty.formParam @Int64 "job_id"
    r <- liftIO $ runPrompt checkpointId prompt
    case r of
        Nothing -> Scotty.redirect "/marker/error"
        Just OrderResult{..} -> do
            _ <- executeDb [sql| UPDATE marker_requests SET request_check_url = ?, status = '' WHERE id = ?; |] (requestCheckUrl, jobId)
            _ <- executeDb [sql| insert into prompt_history (prompt, request_id) values (?, ?); |] (prompt, jobId)
            box <- lift $ asks markerRequest
            liftIO $ putMVar box jobId
            Scotty.html . renderHtml $ do
                renderPoll True True
                div ! customAttribute "hx-swap-oob" "beforeend:#logs" $ p (toMarkup prompt)
                div ! customAttribute "hx-swap-oob" "beforeend:#logs" $ p "💡 Processing..."


saveTemplateHandler :: Handler ()
saveTemplateHandler = do
    prompt <- Scotty.formParam @Text "prompt"
    _ <- executeDb [sql| UPDATE prompt_templates SET prompt = ? WHERE id = 1; |] $ Only prompt
    Scotty.html . renderHtml $ mempty

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

isRefinedHandler :: Handler ()
isRefinedHandler = do
    paramJobId <- Scotty.queryParam @Int64 "job_id"
    curr <- Scotty.queryParam @Int "page_no"
    [Only status] <- queryDb @(Only Int64) @(Only Text) [sql| SELECT status FROM marker_requests WHERE id = ?; |] $ Only paramJobId
    case status of
        "complete" -> do
            docPage <- getSinglePage True paramJobId curr
            Scotty.html . renderHtml $ do
                docPage
                renderPoll False False
                div ! customAttribute "hx-swap-oob" "beforeend:#logs" $ p "✅ Done"
        _ -> Scotty.html . renderHtml $ renderPoll True False

resultPage :: Handler ()
resultPage = do
    paramJobId <- Scotty.captureParam @Int64 "id"
    layout <- layoutM
    [Only checkpointId] <- queryDb @(Only Int64) @(Only Text) [sql| SELECT checkpoint_id FROM marker_requests WHERE id = ?; |] $ Only paramJobId
    [Only total] <- queryDb @(Only Int64) @(Only Int) [sql| SELECT max(page_no) from marker_blocks where request_id = ?; |] $ Only paramJobId
    [Only templ] <- queryDb_ [sql| SELECT prompt FROM prompt_templates WHERE id = 1; |]
    docPage <- getSinglePage False paramJobId 1
    Scotty.html . renderHtml $ layout $ do
        div ! class_ "wrapper" $ do
            div ! id "prompting"  $ promptForm paramJobId checkpointId templ
            div $ do
                navForm False total 1
                docPage
            div $ do
                renderPoll False False
                code ! id "logs" $ do
                    p "Welcome! 🙋"

getSinglePage :: Bool -> Int64 -> Int -> Handler Html
getSinglePage oob jobId currentPage = do
    result <- queryDb [sql|
        SELECT html, page_no FROM marker_blocks WHERE request_id = ? AND page_no = ? AND block_type <> 'Picture' ORDER BY page_no, page_order;
    |] (jobId, currentPage)
    return $ div ! id "document" !? (oob, hx_oob) $ getContent result

navForm :: Bool -> Int -> Int -> Html
navForm oob total curr = form ! id "nav_form" !? (oob, customAttribute "hx_swap" "outerHTML") !? (oob, customAttribute "hx-swap-oob" "true") $ do
    input ! type_ "hidden" ! name "total_pages" ! value (toValue total)
    input ! type_ "hidden" ! name "page_no" ! value (toValue curr)
    div $ do
        button ! customAttribute "hx-post" "/marker/page/prev" ! jobIdAttr !? (curr == 1, disabled "true") $ "Prev"
        span $ toMarkup $ "Page " <> showt curr <> " of " <> showt total
        button ! customAttribute "hx-post" "/marker/page/next" ! jobIdAttr !? (curr == total, disabled "true") $ "Next"
    where
        jobIdAttr = customAttribute "hx-include" "[name='job_id']"

prevPageHandler :: Handler ()
prevPageHandler = do
    total <- Scotty.formParam @Int "total_pages"
    curr <- Scotty.formParam @Int "page_no"
    jobId <- Scotty.formParam @Int64 "job_id"
    let curr' = if curr > 0 then curr - 1 else 0
    docPage <- getSinglePage True jobId curr'
    Scotty.html . renderHtml $ do
        navForm True total curr'
        docPage

nextPageHandler :: Handler ()
nextPageHandler = do
    total <- Scotty.formParam @Int "total_pages"
    curr <- Scotty.formParam @Int "page_no"
    jobId <- Scotty.formParam @Int64 "job_id"
    let curr' = if curr < total then curr + 1 else total 
    docPage <- getSinglePage True jobId curr'
    Scotty.html . renderHtml $ do
        navForm True total curr'
        docPage

getContent :: [(Text, Int)] -> Html
getContent = renderPages . NE.groupWith snd

renderPoll :: Bool -> Bool -> Html
renderPoll active oob
    | not active && not oob = div ! id "poll" $ mempty
    | not active && oob     = div ! id "poll" ! hx_oob $ mempty
    | active && not oob     = div ! id "poll" ! hx_get ! hx_include ! hx_trigger ! hx_swap $ mempty
    | active && oob         = div ! id "poll" ! hx_get ! hx_include ! hx_trigger ! hx_swap ! hx_oob $ mempty
    | otherwise             = div ! id "poll" $ mempty
    where
        hx_get = customAttribute "hx-get" "/marker/is-refined"
        hx_trigger = customAttribute "hx-trigger" "load delay:1s"
        hx_swap = customAttribute "hx-swap" "outerHTML"
        hx_include = customAttribute "hx-include" "[name='job_id'], [name='page_no']"

hx_oob :: Attribute
hx_oob = customAttribute "hx-swap-oob" "true"

renderPages :: [NonEmpty (Text, Int)] -> Html
renderPages = mconcat . fmap renderPage

renderPage :: NonEmpty (Text, Int) -> Html
renderPage sections = article $ do
    let pageNo = snd . NE.head $ sections
    -- header $ small $ text ("Page " <> showt pageNo)
    mapM_ preEscapedToHtml $ fmap fst sections
    footer $ small $  text ("Page " <> showt pageNo)

promptForm :: Int64 -> Text -> Text -> Html
promptForm jobId checkpointId defaultPrompt = form ! customAttribute "hx-post" "/marker/refine" ! customAttribute "hx-swap" "none" $ do
    input ! type_ "hidden" ! name "checkpoint_id" ! value (toValue checkpointId)
    input ! type_ "hidden" ! name "job_id" ! value (toValue jobId)
    label $ do
        "Prompt"
        textarea ! name "prompt" $ text defaultPrompt
    section $ do
        button ! type_ "submit" $ "Run"
        button ! customAttribute "hx-put" "/marker/template" ! customAttribute "hx-swap" "none" $ "Save as template"


-- TODO: to remove probably
-- innerPromptForm :: Int64 -> Text -> Html
-- innerPromptForm jobId checkpointId = do
--     input ! type_ "hidden" ! name "checkpoint_id" ! value (toValue checkpointId)
--     input ! type_ "hidden" ! name "job_id" ! value (toValue jobId)
--     label $ do
--         "Prompt"
--         textarea ! name "prompt" $ text ""
--     button ! type_ "submit" $ "Post"

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

newtype PageOrder = PageOrder Int
    deriving (Show, Eq, Generic)
deriving newtype instance FromField PageOrder
deriving newtype instance ToField PageOrder

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
        Right JobResult{..} -> when (markerStatus == "complete") $ do
            putStrLn "Polling finshed successfully"
            saveStatusComplete connPool jobId
            whenJust checkpointId $ saveCheckpointId connPool jobId
            storeChunks connPool jobId $ maybe [] blocks chunks
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

parsePageNoAndOrder :: Text -> (Maybe Int, Maybe PageOrder)
parsePageNoAndOrder x = bimap (fmap (+1) . convert') (fmap PageOrder . convert') $ getPageAndOrder chunks
    where
        convert' = rightToMaybe . readEither . toString
        chunks = T.split (=='/') x
        getPageAndOrder [_, _, page, _, order] = (page, order)
        getPageAndOrder _ = ("", "")
