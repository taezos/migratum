{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module TestImport
  ( module X
  , TestAppM(..)
  , runTestM'
  ) where

import           Relude                   as X hiding (FilePath)
import           Test.Hspec               as X

import           Control.Monad.Except

import           Migratum.Capability.File as X
import           Migratum.Feedback        as X

import qualified Data.Map                 as M

import           Turtle                   (FilePath)
import qualified Turtle

newtype TestAppM a
  = TestAppM
  { runTestM :: ( ExceptT MigratumError ( State ( M.Map Text Text ) ) ) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadError MigratumError
  , MonadState ( M.Map Text Text )
  )

mkTestDirEff :: MonadState ( M.Map Text Text ) m => FilePath -> m MigratumResponse
mkTestDirEff fp = modify ( M.insert ( filePathToTxt fp ) "" )
  >> ( pure $ Generated ( filePathToTxt fp ) )

mkTestFileEff
  :: MonadState ( M.Map Text Text ) m
  => FilePath
  -> Text
  -> m MigratumResponse
mkTestFileEff fp content = modify ( M.insert ( filePathToTxt fp ) content )
  >> ( pure $ Generated $ filePathToTxt fp )

instance ManageFile TestAppM Text where
  genMigrationDir :: TestAppM MigratumResponse
  genMigrationDir = genMigrationDirImpl mkTestDirEff

  genSqlMigrationDir :: TestAppM MigratumResponse
  genSqlMigrationDir = genSqlMigrationDirImpl mkTestDirEff

  genMigrationConfig :: TestAppM MigratumResponse
  genMigrationConfig = genMigrationConfigImpl mkTestFileEff

filePathToTxt :: FilePath -> Text
filePathToTxt = either id id . Turtle.toText

runTestM' :: M.Map Text Text -> TestAppM a -> Either MigratumError a
runTestM' fileSystem ( TestAppM a ) =
  flip evalState fileSystem . runExceptT $ a
