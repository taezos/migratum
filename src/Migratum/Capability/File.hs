{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Migratum.Capability.File where

import           Import               hiding (FilePath)

-- mtl
import           Control.Monad.Except

-- turtle
import           Turtle               (FilePath)
import qualified Turtle
import qualified Turtle.Prelude       as TP
import           Turtle.Shell         (FoldShell (..))
import qualified Turtle.Shell         as TS

-- migratum
import           Migratum.ConnectInfo
import           Migratum.Feedback

-- yaml
import qualified Data.Yaml            as Y

-- text
import qualified Data.Text.Encoding   as TE

class MonadError MigratumError m => ManageFile m v | m -> v where
  genMigrationDir :: m MigratumResponse
  genSqlMigrationDir :: m MigratumResponse
  genMigrationConfig :: m MigratumResponse
  -- | sql script names must follow the migratum naming convention.
  -- For example,
  --
  -- > V1__my_table.sql
  getMigrationScriptNames :: m [ FilePath ]

-- * Implementations
readDirEff :: MonadIO m => FilePath -> m [ FilePath ]
readDirEff fp = TS.foldShell ( TP.ls fp )
  ( FoldShell (\filePaths filePath -> pure $ filePath : filePaths) empty pure )

mkDirEff
  :: ( MonadIO m, MonadError MigratumError m )
  => FilePath
  -> m MigratumResponse
mkDirEff dirName = do
  isExists <- TP.testdir dirName
  if isExists
    then throwError DirectoryAlreadyExists
    else TP.mkdir dirName
      >> ( pure $ Generated $ either id id $ Turtle.toText dirName )

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
      >> ( pure $ Generated $ either id id $ Turtle.toText filePath )

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
  createFileEff "./migrations/migratum.yaml" ( TE.decodeUtf8 $ Y.encode defaultMigratumConfig )

genSqlMigrationDirImpl
  :: Monad m =>
  ( FilePath -> m MigratumResponse )
  -> m MigratumResponse
genSqlMigrationDirImpl createDirEff = createDirEff "./migrations/sql"

getMigrationScriptNamesImpl
  :: MonadError MigratumError m
  => ( FilePath -> m [ FilePath ] )
  -> m [ FilePath ]
getMigrationScriptNamesImpl readDir = readDir "./migrations/sql"
