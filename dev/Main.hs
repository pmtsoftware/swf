{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Relude

import System.FSNotify
import Control.Concurrent (threadDelay, forkIO, writeChan, newChan, readChan)
import System.Process.Internals (ProcessHandle)
import System.Process (createProcess, shell, readCreateProcess, cleanupProcess)
import System.FilePath (takeFileName, takeExtension)
import qualified Network.WebSockets as WS
import Control.Concurrent.Chan (Chan)
import Control.Exception (finally)
import Data.Time.Clock.System (SystemTime, getSystemTime)
import TextShow (TextShow, showb, showt, fromText)

type ProcDescr = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

data DevEvent = AppBuild | FileChanged !Text
    deriving (Show, Eq)
instance TextShow DevEvent where
    showb AppBuild = "build"
    showb (FileChanged fn) = "file " <> fromText fn

-- we will use timestamp as client id (should be ok for local connections)
type Clients = MVar [(SystemTime, Chan DevEvent)]

main :: IO ()
main = do
    pd <- createProcess $ shell "stack exec swf-exe"
    box <- newMVar pd
    output <- readCreateProcess (shell "stack path --local-install-root") ""
    let outs = lines $ toText output
        dirMaybe = (<> "/bin") <$> viaNonEmpty head outs
        dir = toString $ fromMaybe "." dirMaybe
    clients <- newMVar newClientsState
    _ <- forkIO $ watchAppExe dir box clients
    _ <- forkIO $ watchStatic "./static" clients
    _ <- forkIO $ runDevServer clients
    forever $ threadDelay 1000000

newClientsState :: [(SystemTime, Chan DevEvent)]
newClientsState = []

watchAppExe :: FilePath -> MVar ProcDescr -> Clients -> IO ()
watchAppExe dir box connsBox = withManager $ \mgr -> do
    _ <- watchDir
        mgr
        dir
        filterAppExe
        $ \_ -> do
            pd <- takeMVar box
            putStrLn "Killing swf-exe..."
            cleanupProcess pd
            putStrLn "Starting swf-exe..."
            pd' <- createProcess $ shell "stack exec swf-exe"
            _ <- putMVar box pd'
            conns <- takeMVar connsBox
            forM_ conns $ \(_, clientEvents) -> do
                writeChan clientEvents AppBuild
            putMVar connsBox conns

    forever $ threadDelay 1000000

filterAppExe :: Event -> Bool
filterAppExe (CloseWrite {..}) = takeFileName eventPath == "swf-exe"
filterAppExe _ = False

watchStatic :: FilePath -> Clients -> IO ()
watchStatic dir connsBox = withManager $ \mgr -> do
    _ <- watchDir
        mgr
        dir
        filterStatic
        $ \case
            CloseWrite{..}-> do
                conns <- takeMVar connsBox
                forM_ conns $ \(_, clientEvents) -> do
                    writeChan clientEvents $ FileChanged $ "/static/" <> toText (takeFileName eventPath)
                putMVar connsBox conns
            _ -> return ()

    forever $ threadDelay 1000000

filterStatic :: Event -> Bool
filterStatic (CloseWrite{..}) = takeExtension eventPath == ".css"
filterStatic _ = False

runDevServer :: Clients -> IO ()
runDevServer connsBox = do
    WS.runServer "127.0.0.1" 3069 $ \pConn -> do
        conn <- WS.acceptRequest pConn
        putStrLn "Connected"
        allClients <- takeMVar connsBox
        client <- (,) <$> getSystemTime <*> newChan
        putMVar connsBox $ client : allClients
        run connsBox conn client
    where
        run :: Clients -> WS.Connection -> (SystemTime, Chan DevEvent) -> IO ()
        run allClients conn (clientId, chan) = flip finally (cleanup clientId allClients) $ do
            forever $ readChan chan >>= \e -> WS.sendTextData @Text conn (showt e)

        cleanup :: SystemTime -> Clients -> IO ()
        cleanup clientId allClients = do
            putStrLn "Disconnected"
            cs <- takeMVar allClients
            let cs' = filter (\(cId, _) -> cId /= clientId) cs
            putMVar allClients cs'

