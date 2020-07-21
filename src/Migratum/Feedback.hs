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
  | MigratumError Text
  deriving ( Eq )

-- | Manually deriving Show instance so it will result into
-- > MigratumError Aeson exception:
-- > Error in $.config['postgres_password']: parsing Text failed, expected String, but encountered Null
-- and not
-- > "MigratumGenericError "Aeson exception:\nError in $.config['postgres_password']: parsing Text failed, expected String, but encountered Null"
instance Show MigratumError where
  show NoConfig                 = show NoConfig
  show FileAlreadyExists        = show FileAlreadyExists
  show DirectoryAlreadyExists   = show DirectoryAlreadyExists
  show FileMissing              = show FileMissing
  show ( MigratumError errMsg ) = "MigratumError " <> T.unpack errMsg

data MigrationReadResult = MigrationReadResult
  { _migrationReadResultConnection :: Text
  } deriving ( Eq, Show )

data MigratumResponse
  = Generated Text
  | MigrationPerformed
  | MigrationConfigRead MigrationReadResult
  | MigratumSuccess Text
  deriving ( Eq )

-- | Manually deriving Show instance so it will result into
-- > Generated ./migrations/sql
-- and not
-- > Generate "./migrations/sql"
instance Show MigratumResponse where
  show ( Generated filepath )         = "Generated " <> T.unpack filepath
  show MigrationPerformed             = show MigrationPerformed
  show ( MigrationConfigRead result ) = "MigrationConfigRead "  <> show result
  show ( MigratumSuccess msg )        = "MigrationSuccess " <> T.unpack msg
