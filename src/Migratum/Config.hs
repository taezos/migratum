module Migratum.Config where

-- migratum
import           Import

data Config = Config
  { _configHost :: Text
  } deriving ( Eq, Show )
