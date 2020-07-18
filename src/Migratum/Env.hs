{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Migratum.Env where

import           Import

-- colog
import           Colog                                (HasLog (..), LogAction,
                                                       Message)
-- postgresql-simple
import           Database.PostgreSQL.Simple

-- postgresql-simple-migration
import           Database.PostgreSQL.Simple.Migration

--
import           Migratum.Feedback

data Env m = Env
  { envLogAction       :: LogAction m Message
  , envWithTransaction :: forall a. Connection -> IO a -> m MigratumResponse
  , envRunMigration    :: MigrationContext -> m ( MigrationResult String )
  }

instance HasLog ( Env m ) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction }
