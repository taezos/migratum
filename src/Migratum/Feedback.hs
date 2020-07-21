module Migratum.Feedback where

import           Import    hiding (FilePath, show)

-- base
import           Text.Show

-- text
import qualified Data.Text as T

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
  = Generated Text
  | MigrationPerformed
  | MigrationConfigRead MigrationReadResult
  | MigrationGenericSuccess Text
  deriving ( Eq )

-- | Manually deriving Show instance so it will result into
-- > Generated ./migrations/sql
-- and not
-- > Generate "./migrations/sql"
instance Show MigratumResponse where
  show ( Generated filepath )          = "Generated " <> T.unpack filepath
  show MigrationPerformed              = show MigrationPerformed
  show ( MigrationConfigRead result )  = "MigrationConfigRead "  <> show result
  show ( MigrationGenericSuccess msg ) = "MigrationGenericSuccess " <> T.unpack msg
