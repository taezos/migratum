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
import           Migratum.Capability.File
import           Migratum.Capability.Migration
import           Migratum.Command
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
startApp = do
  comm <- liftIO $ showHelpOnErrorExecParser
    ( info ( helper <*> parseCommand )
      ( fullDesc <> progDesc migratumDesc <> header migratumHeader ))
  res <- runExceptT $ runApp comm
  either ( logError . show ) ( traverse_ ( logInfo . show ) ) res

runApp
  :: MonadIO m
  => Command
  -> ExceptT MigratumError m [ MigratumResponse ]
runApp comm = unAppM $ interpretCli comm

interpretCli :: MonadIO m => Command -> AppM m [ MigratumResponse ]
interpretCli comm = case comm of
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
