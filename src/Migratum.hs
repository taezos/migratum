{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
module Migratum where

-- optparse-applicative
import           Options.Applicative

-- migratum
import           Import
import           Migratum.Capability.CLI
import           Migratum.Capability.File
import           Migratum.Capability.Migration
import           Migratum.ConnectInfo
import           Migratum.Feedback
import           Migratum.Logging

-- mtl
import           Control.Monad.Except

-- hasql
import           Hasql.Connection              (Connection)
import qualified Hasql.Connection              as Connection

-- base
import           Control.Exception

-- yaml
import qualified Data.Yaml                     as Y

-- text
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

-- filepath
import           System.FilePath.Posix


newtype AppM a = AppM
  { unAppM :: ReaderT Env ( ExceptT MigratumError IO ) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadError MigratumError
  , MonadReader Env
  )

data Env = Env
  { envConnection     :: Maybe Connection
  , envMigrationsDir  :: String
  , envScriptsDir     :: String
  , envConfigFileName :: String
  }

startApp :: IO ()
startApp = do
  env <- getAppEnv "migrations" "migratum.yaml"
  either
    ( logError . show )
    ( traverse_ ( logInfo . show ) )
    =<< ( runExceptT $ runReaderT runApp env )

runApp :: ReaderT Env ( ExceptT MigratumError IO ) [ MigratumResponse ]
runApp = unAppM $ interpretCliCommand =<< parseCliCommand

getAppEnv :: FilePath -> FilePath -> IO Env
getAppEnv migrationsDir configFile = do
  res  <- runExceptT $ readFileIO $ joinPath [ migrationsDir, configFile ]
  case res of
    -- case when migratum config file is not yet generated.
    Left _        -> pure $ Env
      { envConnection = Nothing
      , envMigrationsDir = migrationsDir
      , envScriptsDir = "sql"
      , envConfigFileName = configFile
      }
    Right bsContent -> do
      migratumConnect <- configHandler bsContent
      connRes <- Connection.acquire ( toSettings migratumConnect )
      connection <- connectionHandler connRes
      pure $ Env
        { envConnection = Just connection
        , envMigrationsDir = migrationsDir
        , envScriptsDir = "sql"
        , envConfigFileName = configFile
        }
  where
    configHandler bsContent =
      either ( throwIO . MigratumError . T.pack . Y.prettyPrintParseException )
        pure
        $ Y.decodeEither' @MigratumConnect bsContent

    toSettings migConnect =
      mkConnectionSettings . migratumConnectConfig $ migConnect

    connectionHandler :: Either ( Maybe ByteString ) a -> IO a
    connectionHandler connRes = either
      ( throwIO . toMigratumError ) pure
      connRes

    toMigratumError :: Maybe ByteString -> MigratumError
    toMigratumError = maybe
      ( MigratumError "Error acquiring db connection" )
      ( MigratumError . TE.decodeUtf8 )

instance ManageCLI AppM MigratumResponse where
  interpretCliCommand comm = case comm of
    CommandNew -> do
      dirRes <- genMigrationDir
      sqlDir <- genSqlMigrationDir
      fileRes <- genMigrationConfig
      pure [ dirRes, sqlDir, fileRes ]
    CommandInit -> pure <$> initializeMigration
    CommandMigrate -> do
      scriptNames <- getMigrationScriptNames
      runMigratumMigration scriptNames

  parseCliCommand = liftIO $ showHelpOnErrorExecParser
    ( info ( helper <*> parseCommand )
      ( fullDesc <> progDesc migratumDesc <> header migratumHeader ))

instance ManageFile AppM MigratumResponse where
  genMigrationDir = do
    migrationsDir <- asks envMigrationsDir
    mkDirIO migrationsDir

  genMigrationConfig = do
    Env{..} <- ask
    genMigrationConfigImpl mkFileIO
      $ joinPath [ envMigrationsDir, envConfigFileName ]

  genSqlMigrationDir = do
    Env{..} <- ask
    mkDirIO $ joinPath [ envMigrationsDir, envScriptsDir ]

  getMigrationScriptNames = do
    Env{..} <- ask
    readDirIO $ joinPath [ envMigrationsDir, envScriptsDir ]

instance ManageMigration AppM where
  runMigratumMigration :: [ FilePath ] -> AppM [ MigratumResponse ]
  runMigratumMigration fps = do
    mConn <- asks envConnection
    case mConn of
      Nothing -> throwError NoConfig
      Just conn -> runMigratumMigrationImpl conn runTransactionImpl fps

  initializeMigration :: AppM MigratumResponse
  initializeMigration = do
    mConn <- asks envConnection
    case mConn of
      Nothing -> throwError NoConfig
      Just conn -> initializeMigrationImpl conn runTransactionImpl
