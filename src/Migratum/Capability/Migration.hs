{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Migratum.Capability.Migration where

-- migratum
import           Import
import           Migratum.ConnectInfo
import           Migratum.Feedback
import           Migratum.Parser.NamingRule

-- mtl
import           Control.Monad.Except

-- turtle
import qualified Turtle                        as Turtle
import qualified Turtle.Prelude                as TP

-- text
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

-- hasql
import           Hasql.Connection              (Connection, Settings)
import qualified Hasql.Connection              as Connection
import qualified Hasql.Decoders                as D
import qualified Hasql.Encoders                as E
import           Hasql.Session                 (QueryError)
import qualified Hasql.Session                 as Session
import           Hasql.Statement               (Statement (..))
import           Hasql.Transaction             (Transaction)
import qualified Hasql.Transaction             as Transaction
import           Hasql.Transaction.Sessions    (IsolationLevel (..), Mode (..))
import qualified Hasql.Transaction.Sessions    as Transaction

-- parsec
import           Text.ParserCombinators.Parsec (ParseError)

-- cryptonite
import           Crypto.Hash

-- vector
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as V

class Monad m => ManageMigration m where
  initializeMigration :: m MigratumResponse
  runMigratumMigration :: [ FilePath ] -> m [ MigratumResponse ]

readFileIO
  :: ( MonadIO m, MonadError MigratumError m )
  => FilePath
  -> m ByteString
readFileIO filePath = do
  isExists <-  TP.testfile ( Turtle.decodeString filePath )
  if isExists
  then readFileBS filePath
  else throwError FileMissing

-- | the script name field is string because these fields are parsed and
-- validated, and is easier to manipulate as strings, rather than convert back
-- and forth between String and Text
data MigratumScript = MigratumScript
  { migratumScriptFileName    :: ScriptName
  , migratumScriptFileContent :: ByteString
  } deriving ( Eq, Show )

instance Ord MigratumScript where
  compare migratumS1 migratumS2 = compare @(Either String FilenameStructure)
    ( mapLeft show $ parseNamingConvention $ migratumScriptFileName migratumS1 )
    ( mapLeft show $ parseNamingConvention $ migratumScriptFileName migratumS2 )

mapLeft ::( e -> g ) -> Either e a -> Either g a
mapLeft = first

type ScriptName = String

type RunTransactionFn m =
  forall a. Connection -> Transaction a -> m ( Either QueryError a )

mkConnectionSettings :: MigratumConnectInfo -> Settings
mkConnectionSettings MigratumConnectInfo{..} = Connection.settings
  ( TE.encodeUtf8 migratumConnectInfoPostgresHost )
  migratumConnectInfoPostgresPort
  ( TE.encodeUtf8 migratumConnectInfoPostgresUser )
  ( TE.encodeUtf8 migratumConnectInfoPostgresPassword )
  ( TE.encodeUtf8 migratumConnectInfoPostgresDb )

-- | Checks the list of migratum scripts if there are any duplicate version.
-- for example, if it sees V1__person_table.sql and V1__address_table.sql it
-- will return an error.
checkDuplicateVersionsImpl
  :: MonadError MigratumError m
  => [ MigratumScript ]
  -> m [ MigratumScript ]
checkDuplicateVersionsImpl ms = do
  versions <- traverse getVersion
    $ ( parseNamingConvention . migratumScriptFileName ) <$> ms
  if anySame versions
    then throwError $ MigratumError "Duplicate migration version"
    else pure ms
  where
    anySame :: Eq a => [ a ] -> Bool
    anySame = f []

    f :: Eq a => [a] -> [a] -> Bool
    f seen ( x:xs ) = x `elem` seen || f ( x:seen ) xs
    f _ []          = False

-- | Initialize the migration by creating the schema migration table.
initializeMigrationImpl
  :: MonadError MigratumError m
  => Connection
  -> RunTransactionFn m
  -> m MigratumResponse
initializeMigrationImpl conn runTransFn = do
  res <- runTransFn conn $ createSchemaMigrationTable
  either
    ( throwError . MigratumError . show )
    ( const $ pure InitializedMigration ) res

getVersion
  :: MonadError MigratumError m
  => Either ParseError FilenameStructure
  -> m FileVersion
getVersion res = case res of
  Left err -> throwError $ MigratumError $ show err
  Right r  -> pure $ fileVersion r

runTransactionImpl
  :: MonadIO m
  => Connection
  -> Transaction a
  -> m ( Either QueryError a )
runTransactionImpl conn trans = liftIO $
  flip Session.run conn $
    Transaction.transaction
      ReadCommitted
      Write trans

contraZip2 :: E.Params a -> E.Params b -> E.Params ( a, b )
contraZip2 param1 param2 = ( contramap fst $ param1 )
  <> ( contramap snd $ param2 )

toMigratumScript
  :: ( MonadIO m, MonadError MigratumError m )
  => ScriptName
  -> FilePath
  -> m MigratumScript
toMigratumScript scriptName fp = do
  bsContent <- readFileIO fp
  pure $ MigratumScript scriptName bsContent

runMigratumMigrationImpl
  :: ( MonadIO m, MonadError MigratumError m )
  => Connection
  -> RunTransactionFn m
  -> [ FilePath ]
  -> m [ MigratumResponse ]
runMigratumMigrationImpl conn runTransFn filePaths = do
  migratumScripts <- traverse
    ( validateMigratumScriptNaming <=< filePathToMigratumScript )
    filePaths

  checkedForDups <- sort <$> checkDuplicateVersionsImpl migratumScripts
  res <- runTransFn conn $ selectManyScriptsTransaction
  case res of
    Left err -> throwError $ MigratumError $ show err
    Right scripts -> do
      results <- traverse ( transactionHandler conn ) $ executeScriptByStatus
        <$> ( getScriptStatus <$> checkedForDups <*> ( pure $ V.toList scripts ) )
      traverse ( either throwError pure ) results

  where
    transactionHandler connction transRes =
      either ( pure . Left )
        ( responseHandler <=< runTransFn connction ) transRes

    responseHandler res =
      pure $ either
        ( Left .  MigratumError . show )
        ( Right . MigrationSuccess . T.pack )
        res

executeScriptByStatus
  :: ScriptStatus
  -> Either MigratumError ( Transaction String )
executeScriptByStatus status =
  case status of
    ScriptExecuted scriptName    -> Right
      $ pure $ scriptName <> " not altered"

    ScriptToBeExecuted migScript -> Right
      $ T.unpack <$> migrationScriptTransaction migScript

    ScriptAltered scriptName     -> Left
      $ MigratumError $ T.pack scriptName <> " has been altered"

    ScriptInvalid scriptname     -> Left
      $ MigratumError $ T.pack $  scriptname <> "is invalid"

getScriptStatus
  :: MigratumScript
  -- ^ sql scripts about to be executed
  -> [(Text, Text)]
  -- ^ scripts from persisted in the db.
  -> ScriptStatus
getScriptStatus mig dbMigs =
  case isMatch of
    -- need to consider mismatch on checksum
    Nothing           -> ScriptToBeExecuted mig
    Just matchedDbMig -> checkScriptStatus mig matchedDbMig
  where
    isMatch :: Maybe ( Text, Text )
    isMatch = find
      (\(sName, checksum) ->
         -- check if script name and checksum are equal or if script names are
         -- equal but checksums are not
         isScriptNameChecksumMatch sName checksum
         || isChecksumNotMatch sName checksum
      ) dbMigs

    isScriptNameChecksumMatch :: Text -> Text -> Bool
    isScriptNameChecksumMatch sName checksum = sName
      == ( T.pack $ migratumScriptFileName mig )
      && checksum == ( show $ mkChecksum $ migratumScriptFileContent mig )

    isChecksumNotMatch :: Text -> Text -> Bool
    isChecksumNotMatch sName checksum = sName
      == ( T.pack $ migratumScriptFileName mig )
      && checksum /= ( show $ mkChecksum $ migratumScriptFileContent mig )

checkScriptStatus :: MigratumScript -> ( Text, Text ) -> ScriptStatus
checkScriptStatus mig (scriptName, checkSum) =
    -- if migratum script file name is equal to the file name in the database,
    -- and the checksums are equal, then consider the script executed.
  if | scriptName == ( T.pack $ migratumScriptFileName mig )
       && checkSum == ( show $ mkChecksum $ migratumScriptFileContent mig )
       -> ScriptExecuted $ migratumScriptFileName mig

    -- if the script file names are equal but the checksums are not, then
    -- consider the script to have been altered.
     | scriptName == ( T.pack $ migratumScriptFileName mig )
       && checkSum /= ( show $ mkChecksum $ migratumScriptFileContent mig )
       -> ScriptAltered $ migratumScriptFileName mig

    -- if the script names and checksums are not equal, then this script
    -- needs to be executed.
     | scriptName /= ( T.pack $ migratumScriptFileName mig )
       && checkSum /= ( show $ mkChecksum $ migratumScriptFileContent mig )
       -> ScriptToBeExecuted mig

     | otherwise -> ScriptInvalid $ migratumScriptFileName mig

mkChecksum :: ByteString -> Digest SHA256
mkChecksum bsContent = hashWith SHA256 bsContent

scriptToTransaction :: ByteString -> Transaction ()
scriptToTransaction q = Transaction.sql q

data ScriptStatus
  = ScriptExecuted ScriptName
  | ScriptToBeExecuted MigratumScript
  | ScriptAltered ScriptName
  | ScriptInvalid ScriptName
  deriving ( Eq, Show )

filePathToMigratumScript
  :: ( MonadIO m, MonadError MigratumError m )
  => FilePath
  -> m MigratumScript
filePathToMigratumScript fp = do
  bsContent <- readFileIO fp
  pure $ MigratumScript
    ( Turtle.encodeString $ Turtle.filename $ Turtle.decodeString fp )
    bsContent

-- | validates the naming convention.
validateMigratumScriptNaming
  :: MonadError MigratumError m
  => MigratumScript
  -> m MigratumScript
validateMigratumScriptNaming ms = do
  newFilename <- parseHandler
    . parseNamingConvention $ migratumScriptFileName $ ms
  pure $ ms { migratumScriptFileName = newFilename }

-- * SQL

createSchemaMigrationTable :: Transaction ()
createSchemaMigrationTable = Transaction.sql
  "create table if not exists migratum_schema_migrations (\
  \  script_name varchar(250) unique not null,\
  \  checksum varchar(65) unique not null,\
  \  executed_at timestamp without time zone not null default now()\
  \)"

selectManyScriptsTransaction
  :: Transaction ( Vector ( Text, Text ) )
selectManyScriptsTransaction =
  Transaction.statement () selectAllScripts

migrationScriptTransaction :: MigratumScript -> Transaction Text
migrationScriptTransaction MigratumScript{..} = Transaction.statement
  ( T.pack migratumScriptFileName, show $ mkChecksum migratumScriptFileContent )
  insertMigrationScript

insertMigrationScript :: Statement ( Text, Text ) Text
insertMigrationScript = Statement q encoder decoder True
  where
    q :: ByteString
    q = "insert into migratum_schema_migrations (script_name, checksum)\
        \ values ($1,$2) returning script_name"

    encoder :: E.Params ( Text, Text )
    encoder = contraZip2
      ( E.param ( E.nonNullable E.text ) )
      ( E.param ( E.nonNullable E.text ) )

    decoder :: D.Result Text
    decoder = D.singleRow ( D.column $ D.nonNullable D.text )

selectAllScripts :: Statement () ( Vector ( Text, Text ) )
selectAllScripts = Statement q encoder decoder True
  where
    q = "select script_name, checksum from migratum_schema_migrations"
    encoder = E.noParams
    decoder = D.rowVector $ (,)
      <$> D.column ( D.nonNullable D.text )
      <*> D.column ( D.nonNullable D.text )
