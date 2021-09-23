{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
module Migratum where

-- optparse-applicative
import           Options.Applicative

-- turtle
import           Turtle                        (FilePath)

-- migratum
import           Import                        hiding (FilePath)
import           Migratum.Capability.CLI
import           Migratum.Capability.File
import           Migratum.Capability.Migration
import           Migratum.ConnectInfo
import           Migratum.Feedback
import           Migratum.Logging

-- mtl
import           Control.Monad.Except

newtype AppM a
  = AppM
  { unAppM :: ( ExceptT MigratumError IO ) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadError MigratumError
  )

startApp :: IO ()
startApp = either ( logError . show ) ( traverse_ ( logInfo . show ) )
  =<< runExceptT runApp

runApp :: ExceptT MigratumError IO [ MigratumResponse ]
runApp = unAppM $ interpretCliCommand =<< parseCliCommand

instance ManageCLI AppM MigratumResponse where
  interpretCliCommand comm = case comm of
    CommandNew -> do
      dirRes <- genMigrationDir
      sqlDir <- genSqlMigrationDir
      fileRes <- genMigrationConfig
      pure [ dirRes, sqlDir, fileRes ]
    CommandInit -> do
      config <- readMigrationConfig
      pure <$> initializeMigration config
    CommandMigrate -> do
      config <- readMigrationConfig
      scriptNames <- getMigrationScriptNames
      runMigratumMigration config scriptNames

  parseCliCommand = liftIO $ showHelpOnErrorExecParser
    ( info ( helper <*> parseCommand )
      ( fullDesc <> progDesc migratumDesc <> header migratumHeader ))

instance ManageFile AppM MigratumResponse where
  genMigrationDir = genMigrationDirImpl mkDirEff
  genMigrationConfig = genMigrationConfigImpl mkFileEff
  genSqlMigrationDir = genSqlMigrationDirImpl mkDirEff
  getMigrationScriptNames = getMigrationScriptNamesImpl readDirEff

instance ManageMigration AppM MigratumResponse where
  readMigrationConfig :: AppM MigratumConnect
  readMigrationConfig = readMigrationConfigBase readFileEff

  runMigratumMigration
    :: MigratumConnect
    -> [ FilePath ]
    -> AppM [ MigratumResponse ]
  runMigratumMigration = runMigratumMigrationBase runMigrationFns

  initializeMigration :: MigratumConnect -> AppM MigratumResponse
  initializeMigration config = initializeMigrationBase
    acquireConnectionImpl
    runTransaction
    config
