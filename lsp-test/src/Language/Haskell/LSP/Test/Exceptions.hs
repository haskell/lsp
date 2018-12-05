module Language.Haskell.LSP.Test.Exceptions where

import Control.Exception
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B

-- | An exception that can be thrown during a 'Haskell.LSP.Test.Session.Session'
data SessionException = Timeout
                      | UnexpectedMessage String FromServerMessage
                      | ReplayOutOfOrder FromServerMessage [FromServerMessage]
                      | UnexpectedDiagnostics
                      | IncorrectApplyEditRequest String
                      | UnexpectedResponseError LspIdRsp ResponseError
                      | UnexpectedServerTermination
  deriving Eq

instance Exception SessionException

instance Show SessionException where
  show Timeout = "Timed out waiting to receive a message from the server."
  show (UnexpectedMessage expected lastMsg) =
    "Received an unexpected message from the server:\n" ++
    "Was parsing: " ++ expected ++ "\n" ++
    "Last message received: " ++ show lastMsg
  show (ReplayOutOfOrder received expected) =
    let expected' = nub expected
        getJsonDiff = lines . B.unpack . encodePretty
        showExp exp = B.unpack (encodePretty exp) ++ "\nDiff:\n" ++
                ppDiff (getGroupedDiff (getJsonDiff received) (getJsonDiff exp))
    in "Replay is out of order:\n" ++
       -- Print json so its a bit easier to update the session logs
       "Received from server:\n" ++ B.unpack (encodePretty received) ++ "\n" ++
       "Raw from server:\n" ++ B.unpack (encode received) ++ "\n" ++
       "Expected one of:\n" ++ unlines (map showExp expected')
  show UnexpectedDiagnostics = "Unexpectedly received diagnostics from the server."
  show (IncorrectApplyEditRequest msgStr) = "ApplyEditRequest didn't contain document, instead received:\n"
                                          ++ msgStr
  show (UnexpectedResponseError lid e) = "Received an exepected error in a response for id " ++ show lid ++ ":\n"
                                          ++ show e
  show UnexpectedServerTermination = "Language server unexpectedly terminated"

-- | A predicate that matches on any 'SessionException'
anySessionException :: SessionException -> Bool
anySessionException = const True
