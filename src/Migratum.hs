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
import           Migratum.Feedback
import           Migratum.Logging

-- mtl
import           Control.Monad.Except

newtype AppM m a
  = AppM
  { unAppM :: ( ExceptT MigratumError m ) a
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

runApp
  :: MonadIO m
  => ExceptT MigratumError m [ MigratumResponse ]
runApp = unAppM $ interpretCliCommand =<< parseCliCommand

instance MonadIO m => ManageCLI ( AppM m ) MigratumResponse where
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

instance MonadIO m => ManageFile ( AppM m ) MigratumResponse where
  genMigrationDir = genMigrationDirImpl mkDirEff
  genMigrationConfig = genMigrationConfigImpl mkFileEff
  genSqlMigrationDir = genSqlMigrationDirImpl mkDirEff
  getMigrationScriptNames = getMigrationScriptNamesImpl readDirEff

instance MonadIO m => ManageMigration ( AppM m ) MigratumResponse where
  readMigrationConfig :: AppM m Config
  readMigrationConfig = readMigrationConfigBase readFileEff

  runMigratumMigration :: Config -> [ FilePath ] -> AppM m [ MigratumResponse ]
  runMigratumMigration = runMigratumMigrationBase runMigrationFns

  initializeMigration :: Config -> AppM m MigratumResponse
  initializeMigration config = initializeMigrationBase
    acquireConnectionImpl
    runTransaction
    config
