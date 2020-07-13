module FileSpec where

import           TestImport

spec :: Spec
spec = do
  describe "sanity" $ do
    it "will add" $ do
      let
        result = runTestM' mempty $ do
          dirRes <- genMigrationDir
          sqlDir <- genSqlMigrationDir
          fileRes <- genMigrationConfig
          pure [ dirRes, sqlDir, fileRes ]
        expected =
          [ GeneratedDirectory "./migrations"
          , GeneratedDirectory "./migrations/sql"
          , GeneratedFile "./migrations/migratum.yaml"
          ]
      shouldBe result ( Right expected )
