{-# LANGUAGE OverloadedStrings #-}
module Language.LSP.Logging (logToShowMessage, logToLogMessage, defaultClientLogger) where

import Colog.Core
import Language.LSP.Server.Core
import Language.LSP.Types
import Data.Text (Text)

logSeverityToMessageType :: Severity -> MessageType
logSeverityToMessageType sev = case sev of
  Error -> MtError
  Warning -> MtWarning
  Info -> MtInfo
  Debug -> MtLog

-- | Logs messages to the client via @window/logMessage@.
logToLogMessage :: (MonadLsp c m) => LogAction m (WithSeverity Text)
logToLogMessage = LogAction $ \(WithSeverity msg sev) -> do
  sendToClient $ fromServerNot $
    NotificationMessage "2.0" SWindowLogMessage (LogMessageParams (logSeverityToMessageType sev) msg)

-- | Logs messages to the client via @window/showMessage@.
logToShowMessage :: (MonadLsp c m) => LogAction m (WithSeverity Text)
logToShowMessage = LogAction $ \(WithSeverity msg sev) -> do
  sendToClient $ fromServerNot $
    NotificationMessage "2.0" SWindowShowMessage (ShowMessageParams (logSeverityToMessageType sev) msg)

-- | A 'sensible' log action for logging messages to the client:
--
--    * Shows 'Error' logs to the user via @window/showMessage@
--    * Logs 'Info' and above logs in the client via @window/logMessage@
--
-- If you want finer control (e.g. the ability to log 'Debug' logs based on a flag, or similar),
-- then do not use this and write your own based on 'logToShowMessage' and 'logToLogMessage'.
defaultClientLogger :: (MonadLsp c m) => LogAction m (WithSeverity Text)
defaultClientLogger =
  filterBySeverity Error getSeverity logToShowMessage
  <> filterBySeverity Info getSeverity logToLogMessage
