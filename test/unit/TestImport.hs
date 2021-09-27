{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module TestImport
  ( module X
  , TestAppM(..)
  , runTestM'
  ) where

import           Relude                   as X
import           Test.Hspec               as X

import           Control.Monad.Except

import           Migratum.Capability.File as X
import           Migratum.Feedback        as X

import qualified Data.Map                 as M

-- text
import qualified Data.Text                as T

newtype TestAppM a
  = TestAppM
  { runTestM :: ( ExceptT MigratumError ( State ( M.Map Text Text ) ) ) a
  } deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadError MigratumError
  , MonadState ( M.Map Text Text )
  )

mkTestDirEff :: MonadState ( M.Map Text Text ) m => FilePath -> m MigratumResponse
mkTestDirEff fp = modify ( M.insert ( T.pack fp ) "" )
  >> ( pure $ Generated ( T.pack fp ) )

mkTestFileEff
  :: MonadState ( M.Map Text Text ) m
  => FilePath
  -> Text
  -> m MigratumResponse
mkTestFileEff fp content = modify ( M.insert ( T.pack fp ) content )
  >> ( pure $ Generated $ T.pack fp )

readDirTest :: Monad m => FilePath -> m [ FilePath ]
readDirTest _ = pure []

instance ManageFile TestAppM Text where
  genMigrationDir :: TestAppM MigratumResponse
  genMigrationDir = mkTestDirEff "./migrations"

  genSqlMigrationDir :: TestAppM MigratumResponse
  genSqlMigrationDir = mkTestDirEff "./migrations/sql"

  genMigrationConfig :: TestAppM MigratumResponse
  genMigrationConfig = genMigrationConfigImpl mkTestFileEff "./migrations/migratum.yaml"

  getMigrationScriptNames = getMigrationScriptNamesImpl readDirTest

runTestM' :: M.Map Text Text -> TestAppM a -> Either MigratumError a
runTestM' fileSystem ( TestAppM a ) =
  flip evalState fileSystem . runExceptT $ a
