{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Migratum.ConnectInfo where

-- migratum
import           Import

-- aeson
import           Data.Aeson

-- yaml
import           Data.Yaml     (ParseException)
import qualified Data.Yaml     as Y

defaultMigratumConfig :: MigratumConnect
defaultMigratumConfig = MigratumConnect $ MigratumConnectInfo
  mempty
  mempty
  mempty
  mempty
  5432

decodeMigratumConfig :: ByteString -> Either ParseException MigratumConnect
decodeMigratumConfig = Y.decodeEither'

data MigratumConnect = MigratumConnect
  { migratumConnectConfig :: MigratumConnectInfo
  } deriving ( Eq, Show )

data MigratumConnectInfo = MigratumConnectInfo
  { migratumConnectInfoPostgresPassword :: Text
  , migratumConnectInfoPostgresDb       :: Text
  , migratumConnectInfoPostgresUser     :: Text
  , migratumConnectInfoPostgresHost     :: Text
  , migratumConnectInfoPostgresPort     :: Word16
  } deriving ( Eq, Show )

instance ToJSON MigratumConnect where
  toJSON MigratumConnect {..} = object
    [ "config" .= migratumConnectConfig
    ]

instance FromJSON MigratumConnect where
  parseJSON = withObject "migratumConnect" $ \mc -> do
    migratumConnectConfig <- mc .: "config"
    pure $ MigratumConnect {..}

instance ToJSON MigratumConnectInfo where
  toJSON MigratumConnectInfo{..} = object
    [ "postgres_password" .= migratumConnectInfoPostgresPassword
    , "postgres_db" .= migratumConnectInfoPostgresDb
    , "postgres_user" .= migratumConnectInfoPostgresUser
    , "postgres_host" .= migratumConnectInfoPostgresHost
    , "postgres_port" .= migratumConnectInfoPostgresPort
    ]

instance FromJSON MigratumConnectInfo where
  parseJSON = withObject "migratumConnectInfo" $ \mci -> do
    migratumConnectInfoPostgresPassword <- mci .: "postgres_password"
    migratumConnectInfoPostgresDb       <- mci .: "postgres_db"
    migratumConnectInfoPostgresUser     <- mci .: "postgres_user"
    migratumConnectInfoPostgresHost     <- mci .: "postgres_host"
    migratumConnectInfoPostgresPort     <- mci .: "postgres_port"
    pure $ MigratumConnectInfo {..}
