module Migratum.Template where

-- migratum
import           Import

migratumConfig :: Text
migratumConfig = unlines
  [ "config:"
  , "  postgres_password:"
  , "  postgres_db:"
  , "  postgres_user:"
  , "  postgres_host:"
  , "  postgres_port:"
  ]
