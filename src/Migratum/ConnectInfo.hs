{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Migratum.ConnectInfo where

-- migratum
import           Import

-- aeson
import           Data.Aeson

-- microlens
import           Lens.Micro

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
  { _migratumConnectConfig :: MigratumConnectInfo
  } deriving ( Eq, Show )

data MigratumConnectInfo = MigratumConnectInfo
  { _migratumConnectInfoPostgresPassword :: Text
  , _migratumConnectInfoPostgresDb       :: Text
  , _migratumConnectInfoPostgresUser     :: Text
  , _migratumConnectInfoPostgresHost     :: Text
  , _migratumConnectInfoPostgresPort     :: Word16
  } deriving ( Eq, Show )

instance ToJSON MigratumConnect where
  toJSON MigratumConnect {..} = object
    [ "config" .= _migratumConnectConfig
    ]

instance FromJSON MigratumConnect where
  parseJSON = withObject "migratumConnect" $ \mc -> do
    _migratumConnectConfig <- mc .: "config"
    pure $ MigratumConnect {..}

instance ToJSON MigratumConnectInfo where
  toJSON MigratumConnectInfo{..} = object
    [ "postgres_password" .= _migratumConnectInfoPostgresPassword
    , "postgres_db" .= _migratumConnectInfoPostgresDb
    , "postgres_user" .= _migratumConnectInfoPostgresUser
    , "postgres_host" .= _migratumConnectInfoPostgresHost
    , "postgres_port" .= _migratumConnectInfoPostgresPort
    ]

instance FromJSON MigratumConnectInfo where
  parseJSON = withObject "migratumConnectInfo" $ \mci -> do
    _migratumConnectInfoPostgresPassword <- mci .: "postgres_password"
    _migratumConnectInfoPostgresDb       <- mci .: "postgres_db"
    _migratumConnectInfoPostgresUser     <- mci .: "postgres_user"
    _migratumConnectInfoPostgresHost     <- mci .: "postgres_host"
    _migratumConnectInfoPostgresPort     <- mci .: "postgres_port"
    pure $ MigratumConnectInfo {..}

-- * Lens
migratumConnectConfig :: Lens' MigratumConnect MigratumConnectInfo
migratumConnectConfig = lens _migratumConnectConfig
  (\s newConfig -> s { _migratumConnectConfig = newConfig })

migratumConnectInfoPostgresPassword :: Lens' MigratumConnectInfo Text
migratumConnectInfoPostgresPassword = lens _migratumConnectInfoPostgresPassword
  (\s newPass -> s { _migratumConnectInfoPostgresPassword = newPass })

migratumConnectInfoPostgresDb :: Lens' MigratumConnectInfo Text
migratumConnectInfoPostgresDb = lens _migratumConnectInfoPostgresDb
  (\s newDb -> s { _migratumConnectInfoPostgresDb = newDb })

migratumConnectInfoPostgresUser :: Lens' MigratumConnectInfo Text
migratumConnectInfoPostgresUser = lens _migratumConnectInfoPostgresUser
  (\s newUser -> s { _migratumConnectInfoPostgresUser = newUser })

migratumConnectInfoPostgresHost :: Lens' MigratumConnectInfo Text
migratumConnectInfoPostgresHost = lens _migratumConnectInfoPostgresHost
  (\s newHost -> s { _migratumConnectInfoPostgresHost = newHost })

migratumConnectInfoPostgresPort :: Lens' MigratumConnectInfo Word16
migratumConnectInfoPostgresPort = lens _migratumConnectInfoPostgresPort
  (\s newPort -> s { _migratumConnectInfoPostgresPort = newPort })
