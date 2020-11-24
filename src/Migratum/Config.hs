{-# LANGUAGE DeriveGeneric #-}
module Migratum.Config where

-- migratum
import           Import

-- aeson
import           Data.Aeson

-- casing
import           Text.Casing (quietSnake)

data Config = Config
  { _configMigrationConfig :: MigrationConfig
  } deriving ( Eq, Show, Generic )

instance FromJSON Config where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = quietSnake . drop 16 }

data MigrationConfig = MigrationConfig
  { _migrationConfigPostgresPassword :: Text
  , _migrationConfigPostgresDb       :: Text
  , _migrationConfigPostgresUser     :: Text
  , _migrationConfigPostgresHost     :: Text
  , _migrationConfigPostgresPort     :: Word16
  } deriving ( Eq, Show, Generic )

instance FromJSON MigrationConfig where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = quietSnake . drop 16 }
