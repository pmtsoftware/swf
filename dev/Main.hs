module Main (main) where

import Relude

import System.FSNotify
import Control.Concurrent (threadDelay, forkIO)
import System.Process.Internals (ProcessHandle)
import System.Process (createProcess, shell, readCreateProcess, cleanupProcess)
import System.FilePath (takeFileName)
import qualified Network.WebSockets as WS

type ProcDescr = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

type ConnsRef = MVar [WS.Connection]

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
    _ <- forkIO $ runDevServer clients
    forever $ threadDelay 1000000

newClientsState :: [WS.Connection]
newClientsState = []

watchAppExe :: FilePath -> MVar ProcDescr -> ConnsRef -> IO ()
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
            conns <- readMVar connsBox
            forM_ conns $ \conn -> WS.sendTextData @Text conn "reload"

    forever $ threadDelay 1000000

filterAppExe :: Event -> Bool
filterAppExe (CloseWrite {..}) = takeFileName eventPath == "swf-exe"
filterAppExe _ = False

runDevServer :: ConnsRef -> IO ()
runDevServer connsBox = do
    WS.runServer "127.0.0.1" 3069 $ \pConn -> do
        conn <- WS.acceptRequest pConn
        putStrLn "Connection accepted"
        conns <- takeMVar connsBox
        putMVar connsBox $ conn : conns
        putStrLn "Connection stored"
