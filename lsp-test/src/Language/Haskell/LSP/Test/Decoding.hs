{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Decoding where

import           Prelude                 hiding ( id )
import           Data.Aeson
import           Control.Lens
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Maybe
import           System.IO
import           Language.Haskell.LSP.Types
                                         hiding ( error )
import           Language.Haskell.LSP.Messages
import qualified Data.HashMap.Strict           as HM

getAllMessages :: Handle -> IO [B.ByteString]
getAllMessages h = do
  done <- hIsEOF h
  if done
    then return []
    else do
      msg <- getNextMessage h

      (msg :) <$> getAllMessages h

-- | Fetches the next message bytes based on
-- the Content-Length header
getNextMessage :: Handle -> IO B.ByteString
getNextMessage h = do
  headers <- getHeaders h
  case read . init <$> lookup "Content-Length" headers of
    Nothing   -> error "Couldn't read Content-Length header"
    Just size -> B.hGet h size

addHeader :: B.ByteString -> B.ByteString
addHeader content = B.concat
  [ "Content-Length: "
  , B.pack $ show $ B.length content
  , "\r\n"
  , "\r\n"
  , content
  ]

getHeaders :: Handle -> IO [(String, String)]
getHeaders h = do
  l <- hGetLine h
  let (name, val) = span (/= ':') l
  if null val then return [] else ((name, drop 2 val) :) <$> getHeaders h

type RequestMap = HM.HashMap LspId ClientMethod

newRequestMap :: RequestMap
newRequestMap = HM.empty

updateRequestMap :: RequestMap -> RequestMessage ClientMethod a b -> RequestMap
updateRequestMap reqMap msg = HM.insert (msg ^. id) (msg ^. method) reqMap

getRequestMap :: [FromClientMessage] -> RequestMap
getRequestMap = foldl helper HM.empty
 where
  helper acc msg = case msg of
    (ReqInitialize val) -> insert val acc
    (ReqShutdown val) -> insert val acc
    (ReqHover val) -> insert val acc
    (ReqCompletion val) -> insert val acc
    (ReqCompletionItemResolve val) -> insert val acc
    (ReqSignatureHelp val) -> insert val acc
    (ReqDefinition val) -> insert val acc
    (ReqFindReferences val) -> insert val acc
    (ReqDocumentHighlights val) -> insert val acc
    (ReqDocumentSymbols val) -> insert val acc
    (ReqWorkspaceSymbols val) -> insert val acc
    (ReqCodeAction val) -> insert val acc
    (ReqCodeLens val) -> insert val acc
    (ReqCodeLensResolve val) -> insert val acc
    (ReqDocumentFormatting val) -> insert val acc
    (ReqDocumentRangeFormatting val) -> insert val acc
    (ReqDocumentOnTypeFormatting val) -> insert val acc
    (ReqRename val) -> insert val acc
    (ReqExecuteCommand val) -> insert val acc
    (ReqDocumentLink val) -> insert val acc
    (ReqDocumentLinkResolve val) -> insert val acc
    (ReqWillSaveWaitUntil val) -> insert val acc
    _ -> acc
  insert m = HM.insert (m ^. id) (m ^. method)

matchResponseMsgType :: ClientMethod -> B.ByteString -> FromServerMessage
matchResponseMsgType req bytes = case req of
  Initialize                    -> RspInitialize $ fromJust $ decode bytes
  Shutdown                      -> RspShutdown $ fromJust $ decode bytes
  TextDocumentHover             -> RspHover $ fromJust $ decode bytes
  TextDocumentCompletion        -> RspCompletion $ fromJust $ decode bytes
  CompletionItemResolve         -> RspCompletionItemResolve $ fromJust $ decode bytes
  TextDocumentSignatureHelp     -> RspSignatureHelp $ fromJust $ decode bytes
  TextDocumentDefinition        -> RspDefinition $ fromJust $ decode bytes
  TextDocumentReferences        -> RspFindReferences $ fromJust $ decode bytes
  TextDocumentDocumentHighlight -> RspDocumentHighlights $ fromJust $ decode bytes
  TextDocumentDocumentSymbol    -> RspDocumentSymbols $ fromJust $ decode bytes
  WorkspaceSymbol               -> RspWorkspaceSymbols $ fromJust $ decode bytes
  TextDocumentCodeAction        -> RspCodeAction $ fromJust $ decode bytes
  TextDocumentCodeLens          -> RspCodeLens $ fromJust $ decode bytes
  CodeLensResolve               -> RspCodeLensResolve $ fromJust $ decode bytes
  TextDocumentFormatting        -> RspDocumentFormatting $ fromJust $ decode bytes
  TextDocumentRangeFormatting   -> RspDocumentRangeFormatting $ fromJust $ decode bytes
  TextDocumentOnTypeFormatting  -> RspDocumentOnTypeFormatting $ fromJust $ decode bytes
  TextDocumentRename            -> RspRename $ fromJust $ decode bytes
  WorkspaceExecuteCommand       -> RspExecuteCommand $ fromJust $ decode bytes
  TextDocumentDocumentLink      -> RspDocumentLink $ fromJust $ decode bytes
  DocumentLinkResolve           -> RspDocumentLinkResolve $ fromJust $ decode bytes
  TextDocumentWillSaveWaitUntil -> RspWillSaveWaitUntil $ fromJust $ decode bytes
  x                             -> error $ "Not a request: " ++ show x

decodeFromServerMsg :: RequestMap -> B.ByteString -> FromServerMessage
decodeFromServerMsg reqMap bytes =
  case HM.lookup "method" (fromJust $ decode bytes :: Object) of
    Just methodStr -> case fromJSON methodStr of
      Success method -> case method of
        -- We can work out the type of the message
        TextDocumentPublishDiagnostics -> NotPublishDiagnostics $ fromJust $ decode bytes
        WindowShowMessage              -> NotShowMessage $ fromJust $ decode bytes
        WindowLogMessage               -> NotLogMessage $ fromJust $ decode bytes
        CancelRequestServer            -> NotCancelRequestFromServer $ fromJust $ decode bytes
        TelemetryEvent                 -> NotTelemetry $ fromJust $ decode bytes
        WindowShowMessageRequest       -> ReqShowMessage $ fromJust $ decode bytes
        ClientRegisterCapability       -> ReqRegisterCapability $ fromJust $ decode bytes
        ClientUnregisterCapability     -> ReqUnregisterCapability $ fromJust $ decode bytes
        WorkspaceApplyEdit             -> ReqApplyWorkspaceEdit $ fromJust $ decode bytes

      Error e -> error e

    Nothing -> case decode bytes :: Maybe (ResponseMessage Value) of
      Just msg -> case HM.lookup (requestId $ msg ^. id) reqMap of
        Just req -> matchResponseMsgType req bytes -- try to decode it to more specific type
        Nothing  -> error "Couldn't match up response with request"
      Nothing -> error "Couldn't decode message"