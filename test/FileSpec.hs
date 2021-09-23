{-# LANGUAGE TypeApplications #-}
module FileSpec where

-- migratum
import           Migratum.Capability.Migration
import           Migratum.ConnectInfo
import           TestImport

-- lens
import           Control.Lens.Operators

-- yaml
import qualified Data.Yaml     as Y

spec :: Spec
spec = do
  describe "FileSpec" $ do
    it "will generate files" $ do
      let
        result = runTestM' mempty $ do
          dirRes <- genMigrationDir
          sqlDir <- genSqlMigrationDir
          fileRes <- genMigrationConfig
          pure [ dirRes, sqlDir, fileRes ]
        expected =
          [ Generated "./migrations"
          , Generated "./migrations/sql"
          , Generated "./migrations/migratum.yaml"
          ]
      shouldBe result ( Right expected )

    it "will retun an error if there's a duplicate file name" $ do
      let script1 = MigratumScript "V1__uuid_extension.sql" "migrations"
      let script2 = MigratumScript "V1__client_table.sql" "migrations"

      res <- runExceptT $ checkDuplicateImpl [ script1, script2 ]
      shouldBe res ( Left $ MigratumError "Duplicate migration file" )

    it "will decode config file - migratum.yaml" $ do
      fileContent <- readFileBS "test/asset/migratum.yaml"
      case Y.decodeThrow @_ @MigratumConnect fileContent of
        Nothing -> fail "cannot decode config file"
        Just config -> do
          shouldBe
            ( config
              ^. migratumConnectConfig
              . migratumConnectInfoPostgresPassword
            )
            "migratum"

          shouldBe
            ( config
              ^. migratumConnectConfig
              . migratumConnectInfoPostgresDb
            )
            "migratum"

          shouldBe
            ( config
              ^. migratumConnectConfig
              . migratumConnectInfoPostgresUser
            )
            "migratum"

          shouldBe
            ( config
              ^. migratumConnectConfig
              . migratumConnectInfoPostgresHost
            )
            "localhost"

          shouldBe
            ( config
              ^. migratumConnectConfig
              . migratumConnectInfoPostgresPort
            )
            5432
