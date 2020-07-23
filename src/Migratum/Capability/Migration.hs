{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
module Migratum.Capability.Migration where

import           Import                     hiding (FilePath)

-- filepath
import           System.FilePath            (takeFileName)

-- text
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE

-- mtl
import           Control.Monad.Except

-- yaml
import           Data.Yaml

-- turtle
import           Turtle                     (FilePath)
import qualified Turtle
import qualified Turtle.Prelude             as TP

-- hasql
import           Hasql.Connection           (Connection, Settings)
import qualified Hasql.Connection           as Connection
import           Hasql.Session              (QueryError)
import qualified Hasql.Session              as Session
import           Hasql.Transaction          (Transaction)
import           Hasql.Transaction.Sessions (IsolationLevel (..), Mode (..))
import qualified Hasql.Transaction.Sessions as Transaction

-- hasql-migratin
import           Hasql.Migration

-- migratum
import           Migratum.Feedback

class MonadError MigratumError m => ManageMigration m v | m -> v where
  initializeMigration :: Config -> m MigratumResponse
  readMigrationConfig :: m Config
  runMigratumMigration :: Config -> [ FilePath ] -> m [ MigratumResponse ]

readFileEff :: ( MonadIO m, MonadError MigratumError m ) => FilePath -> m Text
readFileEff filePath = do
  isExists <- TP.testfile filePath
  if isExists
  then liftIO $ TP.readTextFile filePath
  else throwError FileMissing

readMigrationConfigImpl
  :: ( Monad m, MonadError MigratumError m )
  => ( FilePath -> m Text )
  -> m Config
readMigrationConfigImpl readEff = do
  config <- readEff "./migrations/migratum.yaml"
  either
    ( throwError . MigratumError . T.pack . prettyPrintParseException )
    pure
    ( decodeEither'
      $ encodeUtf8 config :: Either ParseException Config )

data MigratumScript = MigratumScript
  { migratumScriptFileName :: String
  , migratumScriptFilePath :: String
  } deriving ( Eq, Show )

runMigratumMigrationImpl
  :: ( MonadIO m, MonadError MigratumError m )
  => Config
  -> [ FilePath ]
  -> m [ MigratumResponse ]
runMigratumMigrationImpl Config{..} scriptNames = do
  conn <- either
    ( const $ throwError NoConfig )
    pure
    =<< ( liftIO $ Connection.acquire $ mkConnectionSettings _configMigrationConfig )
  migrationScripts <- liftIO
    $ sequence
    $ (\MigratumScript{..} -> loadMigrationFromFile
        migratumScriptFileName
        migratumScriptFilePath
      )
    <$> ( scriptNameToMigratumScript <$> scriptNames )

  res <- traverse ( runTransaction conn ) $ runMigration <$> migrationScripts
  traverse resHandler  res
  where
    resHandler
      :: ( Monad m, MonadError MigratumError m )
      => Either QueryError ( Maybe MigrationError )
      -> m MigratumResponse
    resHandler res = case res of
      Left err -> throwError . MigratumError . show $ err
      Right mQueryError -> maybe
        ( pure MigrationPerformed )
        ( throwError . MigratumError . show )
        mQueryError

    scriptNameToMigratumScript :: FilePath -> MigratumScript
    scriptNameToMigratumScript =
      (\fp -> MigratumScript ( takeFileName fp ) fp ) . Turtle.encodeString

initializeMigrationImpl
  :: ( MonadIO m, MonadError MigratumError m )
  => Config
  -> m MigratumResponse
initializeMigrationImpl Config{..} = do
  conn <- either
    ( const $ throwError NoConfig )
    pure
    =<< ( liftIO $ Connection.acquire $ mkConnectionSettings _configMigrationConfig )
  res <- runTransaction conn $ runMigration MigrationInitialization
  case res of
    Left err -> throwError . MigratumError $ show err
    Right mig -> maybe
      ( pure InitializedMigration )
      ( throwError . MigratumError . show )
      mig

-- utils
runTransaction
  :: MonadIO m
  => Connection
  -> Transaction a
  -> m ( Either QueryError a )
runTransaction conn trans = liftIO $
  Session.run ( Transaction.transaction ReadCommitted Write trans ) conn

mkConnectionSettings :: MigrationConfig -> Settings
mkConnectionSettings MigrationConfig{..} = Connection.settings
  ( TE.encodeUtf8 _migrationConfigPostgresHost )
  _migrationConfigPostgresPort
  ( TE.encodeUtf8 _migrationConfigPostgresUser )
  ( TE.encodeUtf8 _migrationConfigPostgresPassword )
  ( TE.encodeUtf8 _migrationConfigPostgresDb )
