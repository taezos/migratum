module Migratum.Feedback where

import           Import

data MigratumError
  = NoConfig
  | FileAlreadyExists
  | DirectoryAlreadyExists
  deriving ( Eq, Show )

data MigratumResponse
  = GeneratedFile
  | GeneratedDirectory
  deriving ( Eq, Show )
