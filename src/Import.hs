module Import where

import Relude

import Config

import Text.Pandoc
import Database.PostgreSQL.Simple hiding (fold)
import Database.PostgreSQL.Simple.SqlQQ
import System.FilePath (takeBaseName, dropExtension)

importFile :: FilePath -> IO ()
importFile path = do
    bs <- readFileLBS path
    result <- runIO $ readDocx def bs >>= writeMarkdown opts
    either print save result
    pure ()

    where
        opts :: WriterOptions
        opts = def {
            writerExtensions = extensionsFromList [Ext_grid_tables]
        }
        save :: Text -> IO ()
        save md = do
            _ <- loadAppConfig
            conn <- connectPostgreSQL ""
            [Only docId] <- query conn stmt (takeBaseName path, md)
            putStr "Saved with id: "
            print (docId :: Int64)
            close conn
            let mdFilePath = dropExtension path <> ".md"
            writeFileBS mdFilePath $ encodeUtf8 md
        stmt = [sql|
            INSERT INTO docs (file_name, md) VALUES (?, ?) RETURNING id;
        |]
