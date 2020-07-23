{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Migratum.Env where

-- colog
import           Colog (HasLog (..), LogAction, Message)

data Env m = Env
  { envLogAction       :: LogAction m Message
  }

instance HasLog ( Env m ) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction }
