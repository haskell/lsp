module Language.Haskell.LSP.Test.Exceptions where

import Control.Exception
import Language.Haskell.LSP.Messages

data SessionException = TimeoutException
                      | UnexpectedMessageException String FromServerMessage

instance Exception SessionException

instance Show SessionException where
  show TimeoutException = "Timed out waiting to receive a message from the server."
  show (UnexpectedMessageException expected lastMsg) =
    "Received an unexpected message from the server:\n" ++
    "Expected: " ++ expected ++ "\n" ++
    "Last message accepted: " ++ show lastMsg

anySessionException :: SessionException -> Bool
anySessionException = const True