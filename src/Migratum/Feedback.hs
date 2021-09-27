{-# LANGUAGE DeriveGeneric #-}
module Migratum.Feedback where

-- base
import           Text.Show

-- migratum
import           Import          hiding (FilePath, show)
import           Migratum.ConnectInfo

-- text
import qualified Data.Text       as T

data MigratumError
  = NoConfig
  | FileAlreadyExists
  | DirectoryAlreadyExists
  | FileMissing
  | MigratumError Text
  deriving ( Eq )

instance Exception MigratumError

-- | Manually deriving Show instance so it will result into
-- > MigratumError Aeson exception:
-- > Error in $.config['postgres_password']: parsing Text failed, expected String, but encountered Null
-- and not
-- > "MigratumGenericError "Aeson exception:\nError in $.config['postgres_password']: parsing Text failed, expected String, but encountered Null"
instance Show MigratumError where
  show NoConfig                 = "NoConfig"
  show FileAlreadyExists        = "FileAlreadyExists"
  show DirectoryAlreadyExists   = "DirectoryAlreadyExists"
  show FileMissing              = "FileMissing"
  show ( MigratumError errMsg ) = "MigratumError " <> T.unpack errMsg

newtype MigratumFilename = MigratumFilename Text
  deriving ( Eq, Show )

migratumFileNameToText :: MigratumFilename -> Text
migratumFileNameToText ( MigratumFilename fn ) = fn

data MigratumResponse
  = Generated Text
  | MigrationPerformed MigratumFilename
  | InitializedMigration
  | MigrationConfigRead MigratumConnectInfo
  | MigrationSuccess Text
  deriving ( Eq )

-- | Manually deriving Show instance so it will result into
-- > Generated ./migrations/sql
-- and not
-- > Generate "./migrations/sql"
instance Show MigratumResponse where
  show ( Generated filepath )          = "Generated " <> T.unpack filepath
  show ( MigrationPerformed filename ) = "MigrationPerformed "
    <> ( T.unpack $ migratumFileNameToText filename )
  show ( MigrationConfigRead result )  = "MigrationConfigRead "  <> show result
  show ( MigrationSuccess msg )         = "MigrationSuccess " <> T.unpack msg
  show InitializedMigration            = "InitializedMigration"
