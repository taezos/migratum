{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
module Migratum where

import           Import

-- co-log
import           Colog                    (richMessageAction)

-- mtl
import           Control.Monad.Except

-- optparse-applicative
import           Options.Applicative

-- migratum
import           Migratum.Capability.File
import           Migratum.Command
import           Migratum.Feedback
import           Migratum.Logging

newtype AppM m a
  = AppM
  { unAppM :: ReaderT ( Env ( AppM m ) ) ( ExceptT MigratumError m ) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadError MigratumError
  , MonadReader ( Env ( AppM m ) )
  )

startApp :: IO ()
startApp = do
  comm <- liftIO $ showHelpOnErrorExecParser
    ( info ( helper <*> parseCommand )
      ( fullDesc <> progDesc migratumDesc <> header migratumHeader ))
  res <- runExceptT $ runApp migratumEnv comm
  either ( logError . show ) ( traverse_ ( logInfo . show ) ) res

migratumEnv :: MonadIO m => Env ( AppM m )
migratumEnv = Env
  { envLogAction = richMessageAction
  }

runApp
  :: MonadIO m
  => Env ( AppM m )
  -> Command
  -> ExceptT MigratumError m [ MigratumResponse ]
runApp env comm = runReaderT ( unAppM $ interpretCli comm ) env

interpretCli :: MonadIO m => Command -> AppM m [ MigratumResponse ]
interpretCli comm = case comm of
  CommandInit -> do
    dirRes <- generateMigrationDir
    fileRes <- generateMigrationConfig
    pure [ dirRes, fileRes ]

instance MonadIO m => ManageFile ( AppM m ) where
  generateMigrationDir = genMigrationDir mkDirEff
  generateMigrationConfig = genMigrationConfig mkFileEff
