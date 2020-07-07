{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
module Migratum.Logging where

import           Import

-- text
import qualified Data.Text           as T

-- colog
import           Colog               (HasLog (..), LogAction, Message,
                                      Severity (..))
-- microlens
import           Lens.Micro.Platform

-- ansi-terminal
import qualified System.Console.ANSI as ANSI

data Env m = Env
  { envLogAction :: LogAction m Message
  }

instance HasLog ( Env m ) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction }

data LogMessage = LogMessage
  { _logMessageText   :: Text
  , _logMessageHeader :: Text
  } deriving ( Eq, Show )

data Log = Log
  { _logReason :: Severity
  , _logMsg    :: LogMessage
  } deriving ( Eq, Show )

logMessageHeader :: Lens' LogMessage Text
logMessageHeader = lens _logMessageHeader
  (\logMessage newHeader -> logMessage { _logMessageHeader = newHeader })

logMessageText :: Lens' LogMessage Text
logMessageText = lens _logMessageText
  (\logMessage newMsg -> logMessage { _logMessageText = newMsg })

logMsg :: Lens' Log LogMessage
logMsg = lens _logMsg (\log newLogMsg -> log { _logMsg = newLogMsg })

mkLog :: Monad m => Severity -> Text -> m Log
mkLog reason msg = do
  pure $ Log
    { _logReason = reason
    , _logMsg = LogMessage
      { _logMessageText = msg
      , _logMessageHeader = mkHeader reason
      }
    }
  where
    mkHeader :: Severity -> Text
    mkHeader res = case res of
      Info  -> "[Info]: "
      Error -> "[Error]: "
      _     -> mempty

logInfo :: MonadIO m => Text -> m ()
logInfo msg = terminalLog =<< mkLog Info msg

logError :: MonadIO m => Text -> m ()
logError msg = terminalLog =<< mkLog Error msg

terminalLog :: MonadIO m => Log -> m ()
terminalLog logDesc = do
  liftIO $ ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Dull ( reasonToColor $ _logReason logDesc ) ]
  putStr . T.unpack $ logDesc ^. logMsg . logMessageHeader
  liftIO $ ANSI.setSGR []
  putStrLn . T.unpack $ logDesc ^. logMsg . logMessageText
  where
    reasonToColor :: Severity -> ANSI.Color
    reasonToColor sev = case sev of
      Info  -> ANSI.Green
      Error -> ANSI.Red
      _     -> ANSI.Blue
