module Migratum.Template where

import           Import

migratumConfig :: Text
migratumConfig = unlines
  [ "config:"
  , "  postgres_password:"
  , "  postgres_db:"
  , "  postgres_user:"
  ]
