module FileSpec where

import           TestImport

import           Migratum.Capability.Migration

spec :: Spec
spec = do
  describe "sanity" $ do
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
