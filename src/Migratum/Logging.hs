module Migratum.Logging where

import qualified System.Console.ANSI as ANSI

-- migratum
import           Import

-- text
import qualified Data.Text           as T

data LogMessage = LogMessage
  { logMessageText   :: Text
  , logMessageHeader :: Text
  } deriving ( Eq, Show )

data Log = Log
  { logReason :: Severity
  , logMsg    :: LogMessage
  } deriving ( Eq, Show )

data Severity
  = Info
  | Error
  deriving ( Eq, Show )

mkLog :: Monad m => Severity -> Text -> m Log
mkLog reason msg = do
  pure $ Log
    { logReason = reason
    , logMsg = LogMessage
      { logMessageText = msg
      , logMessageHeader = mkHeader reason
      }
    }
  where
    mkHeader :: Severity -> Text
    mkHeader res = case res of
      Info  -> "[Info]: "
      Error -> "[Error]: "

logInfo :: MonadIO m => Text -> m ()
logInfo msg = terminalLog =<< mkLog Info msg

logError :: MonadIO m => Text -> m ()
logError msg = terminalLog =<< mkLog Error msg

terminalLog :: MonadIO m => Log -> m ()
terminalLog logDesc = do
  liftIO $ ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Dull ( reasonToColor $ logReason logDesc ) ]
  putStr $ T.unpack . logMessageHeader . logMsg $ logDesc
  liftIO $ ANSI.setSGR []
  putStrLn $ T.unpack .  logMessageText . logMsg $ logDesc
  where
    reasonToColor :: Severity -> ANSI.Color
    reasonToColor sev = case sev of
      Info  -> ANSI.Green
      Error -> ANSI.Red
