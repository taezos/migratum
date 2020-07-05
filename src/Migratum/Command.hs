module Migratum.Command where

import           Import

-- optparse-applicative
import           Options.Applicative

data Command
  = CommandInit
  deriving ( Eq, Show )

parseCommand :: Parser Command
parseCommand = subparser $
  ( command "init" $ parseCommandInit `withInfo` "Initialize Migration" )

parseCommandInit :: Parser Command
parseCommandInit = pure CommandInit

withInfo :: Parser Command -> String -> ParserInfo Command
withInfo opts desc = info ( helper <*> opts ) $ progDesc desc

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )

migratumDesc :: String
migratumDesc = "Migratum is a database tool that manages migrations"

migratumHeader :: String
migratumHeader = "migratum: migration tool"
