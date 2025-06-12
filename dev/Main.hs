module Main (main) where

import Relude

import System.FSNotify
import Control.Concurrent (threadDelay)
import System.Process.Internals (ProcessHandle)
import System.Process (createProcess, shell, readCreateProcess, cleanupProcess)
import System.FilePath (takeFileName)

type ProcDescr = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

main :: IO ()
main = do
    pd <- createProcess $ shell "stack exec swf-exe"
    box <- newMVar pd
    output <- readCreateProcess (shell "stack path --local-install-root") ""
    let outs = lines $ toText output
        dirMaybe = (<> "/bin") <$> viaNonEmpty head outs
        dir = toString $ fromMaybe "." dirMaybe
    watch dir box
    return ()

watch :: FilePath -> MVar ProcDescr -> IO ()
watch dir box = withManager $ \mgr -> do
    _ <- watchDir
        mgr
        dir
        filterEvent
        $ \_ -> do
            pd <- takeMVar box
            putStrLn "Killing swf-exe..."
            cleanupProcess pd
            putStrLn "Starting swf-exe..."
            pd' <- createProcess $ shell "stack exec swf-exe"
            _ <- putMVar box pd'
            return ()

    forever $ threadDelay 1000000

filterEvent :: Event -> Bool
filterEvent (CloseWrite {..}) = takeFileName eventPath == "swf-exe"
filterEvent _ = False
