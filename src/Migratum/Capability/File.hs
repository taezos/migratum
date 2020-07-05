module Migratum.Capability.File where

import           Import

-- turtle
import qualified Turtle.Prelude as TP

class Monad m => ManageFile m where
  generateMigrationsDir :: m ()

genMigrations :: MonadIO m => m ()
genMigrations = TP.mkdir "./migration"
