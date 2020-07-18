{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
module Migratum.Capability.Migration where

import           Import                               hiding (FilePath)

import           Control.Exception

-- text
import qualified Data.Text                            as T

-- mtl
import           Control.Monad.Except

-- char
import qualified Data.Char                            as C

-- casing
import           Text.Casing                          (snake)

-- yaml
import           Data.Yaml

-- aeson
import           Data.Aeson

-- turtle
import           Turtle                               (FilePath)
import qualified Turtle.Prelude                       as TP

-- postgresql-simple-migration
import           Database.PostgreSQL.Simple.Migration

-- postgresql-simple
import           Database.PostgreSQL.Simple

-- migratum
import           Migratum.Feedback

class MonadError MigratumError m => ManageMigration m v | m -> v where
  initializeMigration :: Connection -> m MigratumResponse
  readMigrationConfig :: m MigratumResponse
  runMigratumMigration :: Connection -> m MigratumResponse

data MigratumMigrationFile = MigratumMigrationFile
  { _migrationFileConfig :: MigrationConfig
  } deriving ( Eq, Show, Generic )

instance FromJSON MigratumMigrationFile  where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = fmap C.toLower . drop 14 }

data MigrationConfig = MigrationConfig
  { _migrationConfigPostgresPassword :: Text
  , _migrationConfigPostgresDb       :: Text
  , _migrationConfigPostgresUser     :: Text
  , _migrationConfigPostgresHost     :: Text
  } deriving ( Eq, Show, Generic )

instance FromJSON MigrationConfig where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = fmap C.toLower . snake . drop 16 }

readFileEff :: ( MonadIO m, MonadError MigratumError m ) => FilePath -> m Text
readFileEff filePath = do
  isExists <- TP.testfile filePath
  if isExists
  then liftIO $ TP.readTextFile filePath
  else throwError FileMissing

readMigrationConfigImpl
  :: ( Monad m, MonadError MigratumError m )
  => ( FilePath -> m Text )
  -> m MigratumResponse
readMigrationConfigImpl readEff = do
  config <- readEff "./migrations/migratum.yaml"
  either
    ( throwError . MigratumGenericError . show )
    ( pure . MigrationConfigRead . MigrationReadResult . mkUrl )
    ( decodeEither' $ encodeUtf8 config :: Either ParseException MigratumMigrationFile )
  where
    mkUrl :: MigratumMigrationFile -> Text
    mkUrl MigratumMigrationFile{..} = "host="
      <> _migrationConfigPostgresHost _migrationFileConfig
      <> " dbname="
      <> _migrationConfigPostgresDb _migrationFileConfig
      <> " user="
      <> _migrationConfigPostgresUser _migrationFileConfig
      <> " password="
      <> _migrationConfigPostgresPassword _migrationFileConfig

initializeMigrationImpl
  :: ( MonadError MigratumError m, MonadIO m )
  => Connection
  -> m MigratumResponse
initializeMigrationImpl conn = tryIOException
  ( const $ throwError $ MigratumGenericError "initialization failed" )
  ( const $ pure $ MigrationGenericSuccess "initialization success" )
  $ withTransaction conn
    $ runMigration $ MigrationContext MigrationInitialization True conn

runMigrationIOImpl
  :: ( MonadIO m, MonadError MigratumError m )
  => Connection
  -> m MigratumResponse
runMigrationIOImpl conn = do
  res <- liftIO $ withTransaction conn
    $ runMigration
    $ MigrationContext ( MigrationDirectory dirPath ) True conn
  case res of
    MigrationError e -> throwError $ MigratumGenericError $ T.pack e
    MigrationSuccess -> pure MigrationPerformed
  where
    dirPath :: String
    dirPath = "./migrations/sql/"

tryIOException
  :: ( MonadError MigratumError m, MonadIO m )
  => (IOException -> m c)
  -> (b -> m c)
  -> IO b
  -> m c
tryIOException h f = either h f <=< liftIO . try @IOException

withTransactionIO
  :: ( MonadError MigratumError m, MonadIO m )
  => Connection
  -> IO a
  -> m MigratumResponse
withTransactionIO conn mig = tryIOException
  ( const $ throwError FileMissing )
  ( const $ pure MigrationPerformed )
  $ withTransaction conn mig

runMigrationIO
  :: ( MonadError MigratumError m, MonadIO m )
  => MigrationContext
  -> m ( MigrationResult String )
runMigrationIO context = tryIOException
  ( const $ throwError FileMissing )
  pure
  $ runMigration context

runMigrationBase
  :: ( Monad m, MonadError MigratumError m )
  => ( Connection -> m ( MigrationResult String ) -> m ( MigrationResult String ) )
  -> ( MigrationContext -> m ( MigrationResult String ) )
  -> Connection
  -> m MigratumResponse
runMigrationBase transactionImpl runMigrationImpl conn = do
  res <- transactionImpl conn
    $ runMigrationImpl
    $ MigrationContext ( MigrationDirectory dirPath ) True conn
  case res of
    MigrationError e -> throwError $ MigratumGenericError $ T.pack e
    MigrationSuccess -> pure MigrationPerformed
  where
    dirPath :: String
    dirPath = "./migrations/sql/"
