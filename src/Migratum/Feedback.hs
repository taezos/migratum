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
  { _migrationReadResultConnection :: Text
  } deriving ( Eq, Show )

data MigratumResponse
  = GeneratedFile Text
  | GeneratedDirectory Text
  | MigrationPerformed
  | MigrationConfigRead MigrationReadResult
  | MigrationGenericSuccess Text
  deriving ( Eq, Show )
