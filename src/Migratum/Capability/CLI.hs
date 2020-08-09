{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Migratum.Capability.CLI where

import           Control.Monad.Except

import           Migratum.Feedback

import           Options.Applicative

-- migratum
import           Import

class MonadError MigratumError m => ManageCLI m v | m -> v where
  parseCliCommand :: m Command
  interpretCliCommand :: Command -> m [ MigratumResponse ]

data Command
  = CommandNew
  | CommandInit
  | CommandMigrate
  deriving ( Eq, Show )

parseCommand :: Parser Command
parseCommand = subparser $
  ( command "new" $ parseCommandNew `withInfo` "Generate files necessary for migration" )
  <>
  ( command "init" $ parseCommandInit `withInfo` "Initialize database for migration" )
  <>
  ( command "migrate" $ parseCommandMigrate `withInfo` "Perform Migration" )

parseCommandNew :: Parser Command
parseCommandNew = pure CommandNew

parseCommandInit :: Parser Command
parseCommandInit = pure CommandInit

parseCommandMigrate :: Parser Command
parseCommandMigrate = pure CommandMigrate

withInfo :: Parser Command -> String -> ParserInfo Command
withInfo opts desc = info ( helper <*> opts ) $ progDesc desc

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )

migratumDesc :: String
migratumDesc = "Migratum is a database tool that manages migrations"

migratumHeader :: String
migratumHeader = "migratum: migration tool"
