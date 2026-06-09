module CommandLine
    ( Command(..)
    , parseCommand
    ) where

import Relude

import Data.Version (showVersion)
import Options.Applicative
import Paths_swf (version)

-- TODO: we want to support following commands:
-- app     : run web app
-- adduser : create new user
-- rmuser  : remove user
-- passwd  : change user passwords

data Command
    = WebApp Bool
    | AddUser Text Text

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

app :: Parser Command
app = WebApp
    <$> switch
        ( long "dev"
        <> help "Development mode"
        )

cmd :: Parser Command
cmd = subparser
    (  command "app" (info app (progDesc "Run web app"))
    <> command "adduser" (info addUser (progDesc "Add user"))
    )

versioner :: Parser (a -> a)
versioner = simpleVersioner ("swf " <> showVersion version)

opts :: ParserInfo Command
opts = info (cmd <**> versioner <**> helper) idm

parseCommand :: IO Command
parseCommand = execParser opts
