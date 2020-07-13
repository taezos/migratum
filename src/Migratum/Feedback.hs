{-# LANGUAGE DeriveGeneric #-}
module Migratum.Feedback where

import           Import

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
  = GeneratedFile
  | GeneratedDirectory
  | MigrationPerformed
  | MigrationConfigRead MigrationReadResult
  deriving ( Eq, Show )
