{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Migratum where

import           Import

-- mtl
import           Control.Monad.Except

-- optparse-applicative
import           Options.Applicative

-- migratum
import           Migratum.Capability.File
import           Migratum.Command
import           Migratum.Feedback

newtype AppM m a
  = AppM
  { unAppM :: ( ExceptT MigratumError m ) a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadError MigratumError )

startApp :: IO ()
startApp = do
  comm <- showHelpOnErrorExecParser
    ( info ( helper <*> parseCommand )
      ( fullDesc <> progDesc migratumDesc <> header migratumHeader ))
  res <- runExceptT $ runApp comm
  either print pure res

runApp :: MonadIO m => Command -> ExceptT MigratumError m ()
runApp comm = unAppM $ interpretCli comm

interpretCli :: MonadIO m => Command -> AppM m ()
interpretCli comm = case comm of
  CommandInit -> generateMigrationsDir

instance MonadIO m => ManageFile ( AppM m ) where
  generateMigrationsDir = genMigrations
