{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Migratum.Capability.MigrationConfig where

import           Import               hiding (FilePath)

-- mtl
import           Control.Monad.Except

-- char
import qualified Data.Char            as C

-- casing
import           Text.Casing          (snake)

-- yaml
import           Data.Yaml

-- aeson
import           Data.Aeson

-- turtle
import           Turtle               (FilePath)
import qualified Turtle.Prelude       as TP

-- migratum
import           Migratum.Feedback

class MonadError MigratumError m => ManageMigrationConfig m where
  readMigrationConfig :: m MigratumResponse

data MigrationFile = MigrationFile
  { _migrationFileConfig :: MigrationConfig
  } deriving ( Eq, Show, Generic )

instance FromJSON MigrationFile  where
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
    ( decodeEither' $ encodeUtf8 config :: Either ParseException MigrationFile )
  where
    mkUrl :: MigrationFile -> Text
    mkUrl MigrationFile{..} = "host="
      <> _migrationConfigPostgresHost _migrationFileConfig
      <> " dbname="
      <> _migrationConfigPostgresDb _migrationFileConfig
      <> " user="
      <> _migrationConfigPostgresUser _migrationFileConfig
      <> " password="
      <> _migrationConfigPostgresPassword _migrationFileConfig
