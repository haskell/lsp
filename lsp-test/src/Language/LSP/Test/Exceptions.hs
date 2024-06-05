module Language.LSP.Test.Exceptions where

import Control.Exception
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.ByteString.Lazy.Char8 qualified as B
import Data.List
import Language.LSP.Protocol.Message

-- | An exception that can be thrown during a 'Haskell.LSP.Test.Session.Session'
data SessionException
  = Timeout (Maybe FromServerMessage)
  | NoContentLengthHeader
  | UnexpectedMessage String FromServerMessage
  | ReplayOutOfOrder FromServerMessage [FromServerMessage]
  | UnexpectedDiagnostics
  | IncorrectApplyEditRequest String
  | forall m. Show (ErrorData m) => UnexpectedResponseError (LspId m) (TResponseError m)
  | UnexpectedServerTermination
  | IllegalInitSequenceMessage FromServerMessage
  | MessageSendError Value IOError

instance Exception SessionException

instance Show SessionException where
  show (Timeout lastMsg) =
    "Timed out waiting to receive a message from the server."
      ++ case lastMsg of
        Just msg -> "\nLast message received:\n" ++ B.unpack (encodePretty msg)
        Nothing -> mempty
  show NoContentLengthHeader = "Couldn't read Content-Length header from the server."
  show (UnexpectedMessage expected lastMsg) =
    "Received an unexpected message from the server:\n"
      ++ "Was parsing: "
      ++ expected
      ++ "\n"
      ++ "But the last message received was:\n"
      ++ B.unpack (encodePretty lastMsg)
  show (ReplayOutOfOrder received expected) =
    let expected' = nub expected
        getJsonDiff = lines . B.unpack . encodePretty
        showExp exp =
          B.unpack (encodePretty exp)
            ++ "\nDiff:\n"
            ++ ppDiff (getGroupedDiff (getJsonDiff received) (getJsonDiff exp))
     in "Replay is out of order:\n"
          ++
          -- Print json so its a bit easier to update the session logs
          "Received from server:\n"
          ++ B.unpack (encodePretty received)
          ++ "\n"
          ++ "Raw from server:\n"
          ++ B.unpack (encode received)
          ++ "\n"
          ++ "Expected one of:\n"
          ++ unlines (map showExp expected')
  show UnexpectedDiagnostics = "Unexpectedly received diagnostics from the server."
  show (IncorrectApplyEditRequest msgStr) =
    "ApplyEditRequest didn't contain document, instead received:\n"
      ++ msgStr
  show (UnexpectedResponseError lid e) =
    "Received an expected error in a response for id "
      ++ show lid
      ++ ":\n"
      ++ show e
  show UnexpectedServerTermination = "Language server unexpectedly terminated"
  show (IllegalInitSequenceMessage msg) =
    "Received an illegal message between the initialize request and response:\n"
      ++ B.unpack (encodePretty msg)
  show (MessageSendError msg e) =
    "IO exception:\n" ++ show e ++ "\narose while trying to send message:\n" ++ B.unpack (encodePretty msg)

-- | A predicate that matches on any 'SessionException'
anySessionException :: SessionException -> Bool
anySessionException = const True
