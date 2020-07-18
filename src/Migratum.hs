{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module Migratum where

import           Import                        hiding (FilePath)

-- text
import qualified Data.Text.Encoding            as TE

-- co-log
import           Colog                         (richMessageAction)

-- mtl
import           Control.Monad.Except

-- optparse-applicative
import           Options.Applicative

-- postgresql-simple
import           Database.PostgreSQL.Simple

-- migratum
import           Migratum.Capability.File
import           Migratum.Capability.Migration
import           Migratum.Command
import           Migratum.Env
import           Migratum.Feedback
import           Migratum.Logging

newtype AppM m a
  = AppM
  { unAppM :: ReaderT ( Env ( AppM m ) ) ( ExceptT MigratumError m ) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadError MigratumError
  , MonadReader ( Env ( AppM m ) )
  )

startApp :: IO ()
startApp = do
  comm <- liftIO $ showHelpOnErrorExecParser
    ( info ( helper <*> parseCommand )
      ( fullDesc <> progDesc migratumDesc <> header migratumHeader ))
  res <- runExceptT $ runApp migratumEnv comm
  either ( logError . show ) ( traverse_ ( logInfo . show ) ) res

migratumEnv :: MonadIO m => Env ( AppM m )
migratumEnv = Env
  { envLogAction = richMessageAction
  , envWithTransaction = withTransactionIO
  , envRunMigration = runMigrationIO
  }

runApp
  :: MonadIO m
  => Env ( AppM m )
  -> Command
  -> ExceptT MigratumError m [ MigratumResponse ]
runApp env comm = runReaderT ( unAppM $ interpretCli comm ) env

interpretCli :: MonadIO m => Command -> AppM m [ MigratumResponse ]
interpretCli comm = case comm of
  CommandNew -> do
    dirRes <- genMigrationDir
    sqlDir <- genSqlMigrationDir
    fileRes <- genMigrationConfig
    pure [ dirRes, sqlDir, fileRes ]
  CommandInit -> do
    res <- readMigrationConfig
    case res of
      MigrationConfigRead MigrationReadResult{..} -> do
        conn <- liftIO $ connectPostgreSQL ( TE.encodeUtf8 _migrationReadResultConnection )
        pure <$> initializeMigration conn
      _                                           -> throwError NoConfig
  CommandMigrate -> do
    res <- readMigrationConfig
    case res of
      MigrationConfigRead MigrationReadResult{..} -> do
        conn <- liftIO $ connectPostgreSQL ( TE.encodeUtf8 _migrationReadResultConnection )
        pure <$> runMigratumMigration conn
      _                                           -> throwError NoConfig

instance MonadIO m => ManageFile ( AppM m ) MigratumResponse where
  genMigrationDir = genMigrationDirImpl mkDirEff
  genMigrationConfig = genMigrationConfigImpl mkFileEff
  genSqlMigrationDir = genSqlMigrationDirImpl mkDirEff

instance MonadIO m => ManageMigration ( AppM m ) MigratumResponse where
  readMigrationConfig :: AppM m MigratumResponse
  readMigrationConfig = readMigrationConfigImpl readFileEff

  runMigratumMigration :: Connection -> AppM m MigratumResponse
  runMigratumMigration = runMigrationIOImpl

  initializeMigration :: Connection -> AppM m MigratumResponse
  initializeMigration = initializeMigrationImpl
