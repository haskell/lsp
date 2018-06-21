module Language.Haskell.LSP.Test.Exceptions where

import Control.Exception
import Language.Haskell.LSP.Messages

data SessionException = TimeoutException
                      | UnexpectedMessageException String FromServerMessage
                      | ReplayOutOfOrderException FromServerMessage [FromServerMessage]

instance Exception SessionException

instance Show SessionException where
  show TimeoutException = "Timed out waiting to receive a message from the server."
  show (UnexpectedMessageException expected lastMsg) =
    "Received an unexpected message from the server:\n" ++
    "Expected: " ++ expected ++ "\n" ++
    "Last message accepted: " ++ show lastMsg
  show (ReplayOutOfOrderException received expected) =
    "Replay is out of order:\n" ++
    "Received from server:" ++ show received ++ "\n" ++
    "Expected one of: " ++ concatMap show expected

anySessionException :: SessionException -> Bool
anySessionException = const True