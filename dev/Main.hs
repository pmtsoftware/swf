module Main (main) where

import Relude

import System.FSNotify
import Control.Concurrent (threadDelay, forkIO)
import System.Process.Internals (ProcessHandle)
import System.Process (createProcess, shell, readCreateProcess, cleanupProcess)
import System.FilePath (takeFileName)
import qualified Network.WebSockets as WS

type ProcDescr = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

main :: IO ()
main = do
    pd <- createProcess $ shell "stack exec swf-exe"
    box <- newMVar pd
    output <- readCreateProcess (shell "stack path --local-install-root") ""
    let outs = lines $ toText output
        dirMaybe = (<> "/bin") <$> viaNonEmpty head outs
        dir = toString $ fromMaybe "." dirMaybe
    clients <- newMVar newClientsState
    _ <- forkIO $ watchAppExe dir box
    forever $ threadDelay 1000000

newClientsState :: [WS.Connection]
newClientsState = []

watchAppExe :: FilePath -> MVar ProcDescr -> IO ()
watchAppExe dir box = withManager $ \mgr -> do
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
            return ()

    -- forever $ threadDelay 1000000
    return ()

filterAppExe :: Event -> Bool
filterAppExe (CloseWrite {..}) = takeFileName eventPath == "swf-exe"
filterAppExe _ = False

runDevServer :: IO ()
runDevServer = do
    WS.runServer "127.0.0.1" 3069 undefined
    return ()
