{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Migratum.Capability.File where

import           Import

-- mtl
import           Control.Monad.Except

-- turtle
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
readDirIO :: MonadIO m => FilePath -> m [ FilePath ]
readDirIO fp = do
  s <- TS.foldShell ( TP.ls $ Turtle.decodeString fp )
    ( FoldShell (\filePaths filePath -> pure $ filePath : filePaths) empty pure )
  pure $ Turtle.encodeString <$> s

mkDirIO
  :: ( MonadIO m, MonadError MigratumError m )
  => FilePath
  -> m MigratumResponse
mkDirIO dirName = do
  isExists <- TP.testdir $ Turtle.decodeString dirName
  if isExists
    then throwError DirectoryAlreadyExists
    else TP.mkdir ( Turtle.decodeString dirName ) >> pure migratumResponse
  where
    migratumResponse = Generated
      $ either id id
      $ Turtle.toText
      $ Turtle.decodeString dirName

mkFileIO
  :: ( MonadIO m, MonadError MigratumError m )
  => FilePath
  -> Text
  -> m MigratumResponse
mkFileIO filePath content = do
  let decodedFilePath = Turtle.decodeString filePath
  isExists <- TP.testfile decodedFilePath
  if isExists
    then throwError FileAlreadyExists
    else liftIO $ TP.writeTextFile decodedFilePath content
      >> ( pure $ Generated $ either id id $ Turtle.toText decodedFilePath )

genMigrationConfigImpl
  :: Monad m
  => ( FilePath -> Text -> m MigratumResponse )
  -> FilePath
  -> m MigratumResponse
genMigrationConfigImpl createFileFn filePath =
  createFileFn filePath
    ( TE.decodeUtf8 $ Y.encode defaultMigratumConfig )

