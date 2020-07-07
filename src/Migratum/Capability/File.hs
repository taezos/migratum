{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
module Migratum.Capability.File where

import           Import               hiding (FilePath)

-- mtl
import           Control.Monad.Except

-- migratum
import           Migratum.Feedback
import           Migratum.Template

-- turtle
import           Turtle               (FilePath)
import qualified Turtle.Prelude       as TP

class MonadError MigratumError m => ManageFile m where
  generateMigrationDir :: m MigratumResponse
  generateMigrationConfig :: m MigratumResponse

mkDirEff :: ( MonadIO m, MonadError MigratumError m ) => FilePath -> m MigratumResponse
mkDirEff dirName = do
  isExists <- TP.testdir dirName
  if isExists
    then throwError DirectoryAlreadyExists
    else TP.mkdir dirName >> pure GeneratedDirectory

mkFileEff :: ( MonadIO m, MonadError MigratumError m ) => FilePath -> Text -> m MigratumResponse
mkFileEff filePath content = do
  isExists <- TP.testfile filePath
  if isExists
    then throwError FileAlreadyExists
    else liftIO $ TP.writeTextFile filePath content >> pure GeneratedFile

genMigrationDir :: Monad m => ( FilePath -> m MigratumResponse ) -> m MigratumResponse
genMigrationDir createDirEff = createDirEff "./migrations"

genMigrationConfig
  :: Monad m
  => ( FilePath -> Text -> m MigratumResponse )
  -> m MigratumResponse
genMigrationConfig createFileEff = createFileEff "./migrations/migratum.yaml" migratumConfig
