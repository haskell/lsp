module Language.LSP.Client.Exceptions where

import Control.Exception (Exception)
import Data.Aeson (Value, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.List (nub)
import Language.LSP.Types
    ( FromServerMessage
    , ResponseError
    , SomeLspId
    )
import Prelude

-- | An exception that can be thrown during a 'Language.LSP.Client.Session'
data SessionException
    = Timeout (Maybe FromServerMessage)
    | NoContentLengthHeader
    | UnexpectedMessage String FromServerMessage
    | ReplayOutOfOrder FromServerMessage [FromServerMessage]
    | UnexpectedDiagnostics
    | IncorrectApplyEditRequest String
    | UnexpectedResponseError SomeLspId ResponseError
    | UnexpectedServerTermination
    | IllegalInitSequenceMessage FromServerMessage
    | MessageSendError Value IOError
    deriving stock (Eq)

instance Exception SessionException

instance Show SessionException where
    show (Timeout lastMsg) =
        "Timed out waiting to receive a message from the server."
            ++ case lastMsg of
                Just msg -> "\nLast message received:\n" ++ LazyByteString.unpack (encodePretty msg)
                Nothing -> mempty
    show NoContentLengthHeader = "Couldn't read Content-Length header from the server."
    show (UnexpectedMessage expected lastMsg) =
        "Received an unexpected message from the server:\n"
            ++ "Was parsing: "
            ++ expected
            ++ "\n"
            ++ "But the last message received was:\n"
            ++ LazyByteString.unpack (encodePretty lastMsg)
    show (ReplayOutOfOrder received expected) =
        let expected' = nub expected
            getJsonDiff :: FromServerMessage -> [String]
            getJsonDiff = lines . LazyByteString.unpack . encodePretty
            showExp e =
                LazyByteString.unpack (encodePretty e)
                    ++ "\nDiff:\n"
                    ++ ppDiff (getGroupedDiff (getJsonDiff received) (getJsonDiff e))
         in "Replay is out of order:\n"
                ++
                -- Print json so its a bit easier to update the session logs
                "Received from server:\n"
                ++ LazyByteString.unpack (encodePretty received)
                ++ "\n"
                ++ "Raw from server:\n"
                ++ LazyByteString.unpack (encode received)
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
            ++ LazyByteString.unpack (encodePretty msg)
    show (MessageSendError msg e) =
        "IO exception:\n" ++ show e ++ "\narose while trying to send message:\n" ++ LazyByteString.unpack (encodePretty msg)
