{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
module Migratum.Capability.Migration where

import           Import                        hiding (FilePath)

-- base
import           Data.List                     (nub)

-- extra
import           Data.List.Extra               (anySame)

-- parsec
import           Text.ParserCombinators.Parsec (ParseError)

-- text
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

-- mtl
import           Control.Monad.Except

-- yaml
import           Data.Yaml

-- microlens
import           Lens.Micro

-- turtle
import           Turtle                        (FilePath)
import qualified Turtle
import qualified Turtle.Prelude                as TP

-- hasql
import           Hasql.Connection              (Connection, Settings)
import qualified Hasql.Connection              as Connection
import           Hasql.Session                 (QueryError)
import qualified Hasql.Session                 as Session
import           Hasql.Transaction             (Transaction)
import           Hasql.Transaction.Sessions    (IsolationLevel (..), Mode (..))
import qualified Hasql.Transaction.Sessions    as Transaction

-- hasql-migration
import           Hasql.Migration

-- migratum
import           Migratum.Feedback
import           Migratum.Parser.NamingRule

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
  { _migratumScriptFileName :: String
  , _migratumScriptFilePath :: String
  } deriving ( Eq, Show )

migratumScriptFileName :: Lens' MigratumScript String
migratumScriptFileName = lens _migratumScriptFileName
  (\s newFileName -> s { _migratumScriptFileName = newFileName })

migratumScriptFilePath :: Lens' MigratumScript String
migratumScriptFilePath = lens _migratumScriptFilePath
  (\s newFilePath -> s { _migratumScriptFilePath = newFilePath })

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

  validatedMigratumScripts <- sequence
    $ validateMigratumScript
    <$> ( scriptNameToMigratumScript <$> ( sort scriptNames ) )


  scriptsCheckedForDup <- checkDup validatedMigratumScripts

  migrationScripts <- liftIO
    $ sequence $
    (\MigratumScript{..} -> loadMigrationFromFile
        _migratumScriptFileName
        _migratumScriptFilePath
    ) <$> scriptsCheckedForDup


  res <- traverse ( runTransaction conn )
    $ runMigration
    <$> migrationScripts

  traverse (\(t, a) -> resHandler t a) ( nub ( (,) <$> validatedMigratumScripts <*> res ) )

  where
    resHandler
      :: ( Monad m, MonadError MigratumError m )
      => MigratumScript
      -> Either QueryError ( Maybe MigrationError )
      -> m MigratumResponse
    resHandler migScript res = case res of
      Left err -> throwError . MigratumError . show $ err
      Right mQueryError -> maybe
        ( pure
          . MigrationPerformed
          . MigratumFilename
          . T.pack
          $ _migratumScriptFileName migScript )
        ( throwError . MigratumError . show )
        mQueryError

    scriptNameToMigratumScript :: FilePath -> MigratumScript
    scriptNameToMigratumScript fp = MigratumScript
      ( Turtle.encodeString $ Turtle.filename fp )
      ( Turtle.encodeString fp )

    validateMigratumScript :: MonadError MigratumError m => MigratumScript -> m MigratumScript
    validateMigratumScript ms = do
      newFilename <-  ms
        ^. migratumScriptFileName
        & parseHandler
        . parseNamingConvention
      pure $ ms & migratumScriptFileName .~ newFilename

    checkDup
      :: ( MonadError MigratumError m, MonadIO m )
      => [ MigratumScript ]
      -> m [ MigratumScript ]
    checkDup ms = do
      versions <- traverse namingCoventionHandler
        $ (\s -> s ^. migratumScriptFileName & parseNamingConvention)
        <$> ms
      if anySame versions
        then throwError $ MigratumError "Duplicate migration file"
        else pure ms

    namingCoventionHandler
      :: MonadError MigratumError m
      => Either ParseError FilenameStructure
      -> m Text
    namingCoventionHandler res = case res of
      Left err -> throwError $ MigratumError $ show err
      Right r  -> pure $ _filenameStructureVersion r

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

-- * Utils
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

