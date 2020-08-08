{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Migratum.Capability.Migration where

import           Import                        hiding (FilePath)

-- base
import           Data.List                     (nub)

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
import           Hasql.Connection              (Connection, ConnectionError,
                                                Settings)
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
  isExists <-  TP.testfile filePath
  if isExists
  then liftIO $ TP.readTextFile filePath
  else throwError FileMissing

readMigrationConfigBase
  :: MonadError MigratumError m
  => ( FilePath -> m Text )
  -> m Config
readMigrationConfigBase readEff = do
  config <- readEff "./migrations/migratum.yaml"
  either
    ( throwError . MigratumError . T.pack . prettyPrintParseException )
    pure
    ( decodeEither'
      $ encodeUtf8 config :: Either ParseException Config )

-- | the fields are string because these fields are parsed and validated, and is
-- easier to manipulate as strings, rather than convert back and forth between
-- String and Text
data MigratumScript = MigratumScript
  { _migratumScriptFileName :: String
  , _migratumScriptFilePath :: String
  } deriving ( Eq, Show, Ord )

migratumScriptFileName :: Lens' MigratumScript String
migratumScriptFileName = lens _migratumScriptFileName
  (\s newFileName -> s { _migratumScriptFileName = newFileName })

migratumScriptFilePath :: Lens' MigratumScript String
migratumScriptFilePath = lens _migratumScriptFilePath
  (\s newFilePath -> s { _migratumScriptFilePath = newFilePath })

type CheckDuplicateFn m = [ MigratumScript ] -> m [ MigratumScript ]
type LoadMigrationFromFileFn m = ScriptName -> String -> m MigrationCommand
type RunTransactionFn m a = Connection -> Transaction a -> m ( Either QueryError a )
type AcquireConnectionFn m = Settings -> m ( Either ConnectionError Connection )

data RunMigrationFn m = RunMigrationFn
  { checkDupFn :: [ MigratumScript ] -> m [ MigratumScript ]
  , loadMigrationFromFileFn :: ScriptName -> String -> m MigrationCommand
  , acquireConnectionFn ::  Settings -> m ( Either ConnectionError Connection )
  , runTransactionFn :: Connection -> Transaction ( Maybe MigrationError ) -> m ( Either QueryError ( Maybe MigrationError ) )
  }

runMigrationFns :: ( MonadIO m, MonadError MigratumError m ) => RunMigrationFn m
runMigrationFns = RunMigrationFn
  { checkDupFn = checkDupImpl
  , loadMigrationFromFileFn = (\scriptName filePath -> liftIO $ loadMigrationFromFile scriptName filePath )
  , acquireConnectionFn = acquireConnectionImpl
  , runTransactionFn = runTransaction
  }

runMigratumMigrationBase
  :: MonadError MigratumError m
  => RunMigrationFn m
  -> Config
  -> [ FilePath ]
  -> m [ MigratumResponse ]
runMigratumMigrationBase RunMigrationFn{..} Config{..} scriptNames = do
  -- acquiring connection
  conn <- either
    ( const $ throwError NoConfig )
    pure
    =<< ( acquireConnectionFn $ mkConnectionSettings _configMigrationConfig )

  -- validating the file names of the script if they are following the
  -- established naming convention.
  validatedMigratumScripts <- sequence
    $ validateMigratumScript
    <$> ( scriptNameToMigratumScript <$> ( sort scriptNames ) )

  -- validating that scripts are unique.
  scriptsCheckedForDup <- checkDupFn validatedMigratumScripts

  migrationScripts <- sequence
    $ (\MigratumScript{..} -> loadMigrationFromFileFn
        _migratumScriptFileName
        _migratumScriptFilePath
    ) <$> scriptsCheckedForDup

  res <- traverse ( runTransactionFn conn )
    $ runMigration
    <$> migrationScripts

  traverse (\(t, a) -> resHandler t a) ( nub ( (,) <$> validatedMigratumScripts <*> res ) )

  where
    resHandler
      :: MonadError MigratumError m
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

-- | creates `MigratumScript` from the `FilePath`
scriptNameToMigratumScript :: FilePath -> MigratumScript
scriptNameToMigratumScript fp = MigratumScript
  ( Turtle.encodeString $ Turtle.filename fp )
  ( Turtle.encodeString fp )

-- | validates the naming convention.
validateMigratumScript
  :: MonadError MigratumError m
  => MigratumScript
  -> m MigratumScript
validateMigratumScript ms = do
  newFilename <-  ms
    ^. migratumScriptFileName
    & parseHandler
    . parseNamingConvention
  pure $ ms & migratumScriptFileName .~ newFilename

namingConventionHandler
  :: MonadError MigratumError m
  => Either ParseError FilenameStructure
  -> m Text
namingConventionHandler res = case res of
  Left err -> throwError $ MigratumError $ show err
  Right r  -> r ^. filenameStructureVersion & pure

initializeMigrationBase
  :: MonadError MigratumError m
  => AcquireConnectionFn m
  -> RunTransactionFn m ( Maybe MigrationError )
  -> Config
  -> m MigratumResponse
initializeMigrationBase acquireConnection runTransactionFn Config{..} = do
  conn <- either
    ( const $ throwError NoConfig )
    pure
    =<< ( acquireConnection $ mkConnectionSettings _configMigrationConfig )
  res <- runTransactionFn conn $ runMigration MigrationInitialization
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

acquireConnectionImpl
  :: MonadIO m
  => Settings
  -> m ( Either ConnectionError Connection )
acquireConnectionImpl = liftIO . Connection.acquire

checkDupImpl
  :: MonadError MigratumError m
  => [ MigratumScript ]
  -> m [ MigratumScript ]
checkDupImpl ms = do
  versions <- traverse namingConventionHandler
    $ (\s -> s ^. migratumScriptFileName & parseNamingConvention)
    <$> ms
  if anySame versions
    then throwError $ MigratumError "Duplicate migration file"
    else pure ms
  where
    anySame :: Eq a => [a] -> Bool
    anySame = f []
      where
        f seen ( x:xs ) = x `elem` seen || f ( x:seen ) xs
        f _ []          = False
