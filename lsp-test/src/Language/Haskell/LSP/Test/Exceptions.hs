module Language.Haskell.LSP.Test.Exceptions where

import Control.Exception
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

data SessionException = TimeoutException
                      | UnexpectedMessageException String FromServerMessage
                      | ReplayOutOfOrderException FromServerMessage [FromServerMessage]
                      | UnexpectedDiagnosticsException
                      | IncorrectApplyEditRequestException String
                      | UnexpectedResponseError LspIdRsp ResponseError

instance Exception SessionException

instance Show SessionException where
  show TimeoutException = "Timed out waiting to receive a message from the server."
  show (UnexpectedMessageException expected lastMsg) =
    "Received an unexpected message from the server:\n" ++
    "Expected: " ++ expected ++ "\n" ++
    "Last message accepted: " ++ show lastMsg
  show (ReplayOutOfOrderException received expected) =
    "Replay is out of order:\n" ++
    -- Print json so its a bit easier to update the session logs
    "Received from server:\n" ++ B.unpack (encode received) ++ "\n" ++
    "Expected one of:\n" ++ unlines (map (B.unpack . encode) expected)
  show UnexpectedDiagnosticsException = "Unexpectedly received diagnostics from the server."
  show (IncorrectApplyEditRequestException msgStr) = "ApplyEditRequest didn't contain document, instead received:\n"
                                          ++ msgStr
  show (UnexpectedResponseError lid e) = "Received an exepected error in a response for id " ++ show lid ++ ":\n"
                                          ++ show e

anySessionException :: SessionException -> Bool
anySessionException = const True