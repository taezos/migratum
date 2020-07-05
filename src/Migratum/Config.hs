module Migratum.Config where

import           Import

data Config = Config
  { _configHost :: Text
  } deriving ( Eq, Show )
