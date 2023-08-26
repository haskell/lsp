module Language.LSP.Logging (logToShowMessage, logToLogMessage, defaultClientLogger) where

import Colog.Core
import Data.Text (Text)
import JSONRPC.Typed.Method
import JSONRPC.Typed.RPC
import JSONRPC.Typed.Server
import Language.LSP.MethodInstance ()
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types

logSeverityToMessageType :: Severity -> MessageType
logSeverityToMessageType sev = case sev of
  Error -> MessageType_Error
  Warning -> MessageType_Warning
  Info -> MessageType_Info
  Debug -> MessageType_Log

-- | Logs messages to the client via @window/logMessage@.
logToLogMessage :: ServerHandle Server LSP.Method -> LogAction IO (WithSeverity Text)
logToLogMessage h = LogAction $ \(WithSeverity msg sev) -> do
  sendNotification h.rpcHandle SMethod_WindowLogMessage (LogMessageParams (logSeverityToMessageType sev) msg)

-- | Logs messages to the client via @window/showMessage@.
logToShowMessage :: ServerHandle Server LSP.Method -> LogAction IO (WithSeverity Text)
logToShowMessage h = LogAction $ \(WithSeverity msg sev) -> do
  sendNotification h.rpcHandle SMethod_WindowShowMessage (ShowMessageParams (logSeverityToMessageType sev) msg)

{- | A 'sensible' log action for logging messages to the client:

    * Shows 'Error' logs to the user via @window/showMessage@
    * Logs 'Info' and above logs in the client via @window/logMessage@

 If you want finer control (e.g. the ability to log 'Debug' logs based on a flag, or similar),
 then do not use this and write your own based on 'logToShowMessage' and 'logToLogMessage'.
-}
defaultClientLogger :: ServerHandle Server LSP.Method -> LogAction IO (WithSeverity Text)
defaultClientLogger handle =
  filterBySeverity Error getSeverity (logToShowMessage handle)
    <> filterBySeverity Info getSeverity (logToLogMessage handle)
