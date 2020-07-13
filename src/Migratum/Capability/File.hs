{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Migratum.Capability.File where

import           Import               hiding (FilePath)

-- mtl
import           Control.Monad.Except

-- turtle
import           Turtle               (FilePath)
import qualified Turtle
import qualified Turtle.Prelude       as TP

-- migratum
import           Migratum.Feedback
import           Migratum.Template

-- class MonadError MigratumError m => ManageFile m v | m -> v where
class Monad m => ManageFile m where
  genMigrationDir :: m MigratumResponse
  genSqlMigrationDir :: m MigratumResponse
  genMigrationConfig :: m MigratumResponse

mkDirEff
  :: ( MonadIO m, MonadError MigratumError m )
  => FilePath
  -> m MigratumResponse
mkDirEff dirName = do
  isExists <- TP.testdir dirName
  if isExists
    then throwError DirectoryAlreadyExists
    else TP.mkdir dirName
      >> ( pure $ GeneratedDirectory $ either id id $ Turtle.toText dirName )

mkFileEff
  :: ( MonadIO m, MonadError MigratumError m )
  => FilePath
  -> Text
  -> m MigratumResponse
mkFileEff filePath content = do
  isExists <- TP.testfile filePath
  if isExists
    then throwError FileAlreadyExists
    else liftIO $ TP.writeTextFile filePath content
      >> ( pure $ GeneratedFile $ either id id $ Turtle.toText filePath )

genMigrationDirImpl
  :: Monad m
  => ( FilePath -> m MigratumResponse )
  -> m MigratumResponse
genMigrationDirImpl createDirEff = createDirEff "./migrations"

genMigrationConfigImpl
  :: Monad m
  => ( FilePath -> Text -> m MigratumResponse )
  -> m MigratumResponse
genMigrationConfigImpl createFileEff =
  createFileEff "./migrations/migratum.yaml" migratumConfig

genSqlMigrationDirImpl
  :: Monad m =>
  ( FilePath -> m MigratumResponse )
  -> m MigratumResponse
genSqlMigrationDirImpl createDirEff = createDirEff "./migrations/sql"
