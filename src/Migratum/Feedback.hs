module Migratum.Feedback where

import           Import

data MigratumError
  = NoConfig
  | FileAlredyExists
  deriving ( Eq, Show )
