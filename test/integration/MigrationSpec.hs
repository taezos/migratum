{-# LANGUAGE RecordWildCards #-}
module MigrationSpec where

-- migratum
import           Migratum
import           Migratum.Capability.File
import           Migratum.Capability.Migration
import           Migratum.Feedback
import           TestImport

spec :: Spec
spec = do
  let
    isMigrationSuccess :: MigratumResponse -> Bool
    isMigrationSuccess mig =
      case mig of
        MigrationSuccess _txt -> True
        _                     -> False

  describe "MigrationSpec" $ do
    it "will create migratum_schema_migrations table" $ do
      Env{..} <- getAppEnv "test/asset/migrations" "migratum.yaml"
      case envConnection of
        Nothing -> error "no connection found"
        Just conn -> do
          res <- runTransactionImpl conn $ do
            createSchemaMigrationTable
            selectMigratumTable
          shouldBe res ( Right "migratum_schema_migrations" )

    it "will initialize the database" $ do
      Env{..} <- getAppEnv "test/asset/migrations" "migratum.yaml"
      case envConnection of
        Nothing -> error "no connection found"
        Just conn -> do
          res <- runExceptT $ initializeMigrationImpl conn runTransactionImpl
          shouldBe res ( Right InitializedMigration )

    it "will migrate the database with the scripts in sql directory" $ do
      Env{..} <- getAppEnv "test/asset/migrations" "migratum.yaml"
      migrations <- readDirIO "test/asset/migrations/sql/"
      case envConnection of
        Nothing -> error "no connection found"
        Just conn -> do
          res <- runExceptT
            $ runMigratumMigrationImpl conn runTransactionImpl migrations
          shouldBe ( all (==True) . fmap ( isMigrationSuccess ) <$> res )
            $ Right True
