module Migratum.Feedback where

import           Import hiding (FilePath)

data MigratumError
  = NoConfig
  | FileAlreadyExists
  | DirectoryAlreadyExists
  | FileMissing
  | MigratumGenericError Text
  deriving ( Eq, Show )

data MigrationReadResult = MigrationReadResult
  { _migrationReadResultUrl :: Text
  } deriving ( Eq, Show )

data MigratumResponse
  = GeneratedFile Text
  | GeneratedDirectory Text
  | MigrationPerformed
  | MigrationConfigRead MigrationReadResult
  deriving ( Eq, Show )
