{-# LANGUAGE TemplateHaskell #-}
module Migratum.ConnectInfo where

-- base
import           Data.Char     (toLower)

-- migratum
import           Import

-- aeson
import           Data.Aeson.TH

-- casing
import           Text.Casing   (snake)

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
  mempty

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
  , _migratumConnectInfoPostgresPort     :: Text
  } deriving ( Eq, Show )

$(deriveJSON
  defaultOptions
  { fieldLabelModifier = fmap toLower
    . snake
    . drop ( genericLength "_MigratumConnect" )
  } ''MigratumConnect)

$(deriveJSON
  defaultOptions
  { fieldLabelModifier = fmap toLower
    . snake
    . drop ( genericLength "_MigratumConnectInfo" )
  } ''MigratumConnectInfo)

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

migratumConnectInfoPostgresPort :: Lens' MigratumConnectInfo Text
migratumConnectInfoPostgresPort = lens _migratumConnectInfoPostgresPort
  (\s newPort -> s { _migratumConnectInfoPostgresPort = newPort })
