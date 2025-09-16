module CommandLine
    ( Command(..)
    , parseCommand
    ) where

import Relude

import Options.Applicative

-- TODO: we want to support following commands:
-- app     : run web app
-- adduser : create new user
-- rmuser  : remove user
-- passwd  : change user passwords

data Command
    = App
    | AddUser Text Text
    | Import FilePath

addUser :: Parser Command
addUser = AddUser
    <$> strOption
        ( long "username"
        <> short 'u'
        <> metavar "USERNAME"
        <> help "User name"
        )
    <*> strOption
        ( long "password"
        <> short 'p'
        <> metavar "PASSWORD"
        <> help "Password"
        )

importDoc :: Parser Command
importDoc = Import
    <$> strOption
        ( long "file"
        <> short 'f'
        <> metavar "FILENAME"
        <> help "File to import"
        )

cmd :: Parser Command
cmd = subparser
    (  command "app" (info (pure App) (progDesc "Run web app"))
    <> command "adduser" (info addUser (progDesc "Add user"))
    <> command "import" (info importDoc (progDesc "Import docx documents and convert to md"))
    )

opts :: ParserInfo Command
opts = info (cmd <**> helper) idm

parseCommand :: IO Command
parseCommand = execParser opts
