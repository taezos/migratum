module TestImport
  ( module X
  , selectMigratumTable
  ) where

-- relude
import           Relude            as X

-- hspec
import           Test.Hspec        as X

-- hasql
import qualified Hasql.Decoders    as D
import qualified Hasql.Encoders    as E
import           Hasql.Statement   (Statement (..))
import           Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as Transaction

selectMigratumTable :: Transaction Text
selectMigratumTable = Transaction.statement ()
  $  Statement q encoder decoder True
  where
    q = "select table_name \
        \from information_schema.tables \
        \where table_name = 'migratum_schema_migrations'"
    encoder = E.noParams
    decoder = D.singleRow ( D.column $ D.nonNullable D.text )
