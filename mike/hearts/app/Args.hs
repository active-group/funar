module Args where

import Options.Applicative
import System.Environment

data ServerType = Player | Table
  deriving (Show, Read)

data Args = Args
  { port :: Int,
    serverType :: ServerType,
    playerName :: Maybe String
  }

args :: Parser Args
args =
  Args
    <$> option auto (long "port" <> short 'p' <> help "The port to run on")
    <*> option auto (long "server-type" <> short 's' <> help "The type of the server (Player/Table)")
    <*> optional (strOption (long "player-name" <> short 'n' <> help "The player name"))

opts :: ParserInfo Args
opts = info (args <**> helper) (fullDesc <> progDesc "Starts a game server for a player or the 'table'")

preferences = prefs (showHelpOnEmpty <> showHelpOnError)

parseArgs :: IO Args
parseArgs = customExecParser preferences opts
