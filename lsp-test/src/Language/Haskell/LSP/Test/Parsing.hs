{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Parsing where

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
import           Debug.Trace

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

type RequestMap = HM.HashMap LspId FromClientMessage

getRequestMap :: [FromClientMessage] -> RequestMap
getRequestMap = foldl helper HM.empty
 where
  helper acc msg = case msg of
    (ReqInitialize val) -> insert val msg acc
    (ReqShutdown val) -> insert val msg acc
    (ReqHover val) -> insert val msg acc
    (ReqCompletion val) -> insert val msg acc
    (ReqCompletionItemResolve val) -> insert val msg acc
    (ReqSignatureHelp val) -> insert val msg acc
    (ReqDefinition val) -> insert val msg acc
    (ReqFindReferences val) -> insert val msg acc
    (ReqDocumentHighlights val) -> insert val msg acc
    (ReqDocumentSymbols val) -> insert val msg acc
    (ReqWorkspaceSymbols val) -> insert val msg acc
    (ReqCodeAction val) -> insert val msg acc
    (ReqCodeLens val) -> insert val msg acc
    (ReqCodeLensResolve val) -> insert val msg acc
    (ReqDocumentFormatting val) -> insert val msg acc
    (ReqDocumentRangeFormatting val) -> insert val msg acc
    (ReqDocumentOnTypeFormatting val) -> insert val msg acc
    (ReqRename val) -> insert val msg acc
    (ReqExecuteCommand val) -> insert val msg acc
    (ReqDocumentLink val) -> insert val msg acc
    (ReqDocumentLinkResolve val) -> insert val msg acc
    (ReqWillSaveWaitUntil val) -> insert val msg acc
    _ -> acc
  insert m = HM.insert (m ^. id)

matchResponseMsgType :: FromClientMessage -> B.ByteString -> FromServerMessage
matchResponseMsgType req bytes = case req of
  ReqInitialize _ -> RspInitialize $ fromJust $ decode bytes
  ReqShutdown   _ -> RspShutdown $ fromJust $ decode bytes
  ReqHover      _ -> RspHover $ fromJust $ decode bytes
  ReqCompletion _ -> RspCompletion $ fromJust $ decode bytes
  ReqCompletionItemResolve _ ->
    RspCompletionItemResolve $ fromJust $ decode bytes
  ReqSignatureHelp      _ -> RspSignatureHelp $ fromJust $ decode bytes
  ReqDefinition         _ -> RspDefinition $ fromJust $ decode bytes
  ReqFindReferences     _ -> RspFindReferences $ fromJust $ decode bytes
  ReqDocumentHighlights _ -> RspDocumentHighlights $ fromJust $ decode bytes
  ReqDocumentSymbols    _ -> RspDocumentSymbols $ fromJust $ decode bytes
  ReqWorkspaceSymbols   _ -> RspWorkspaceSymbols $ fromJust $ decode bytes
  ReqCodeAction         _ -> RspCodeAction $ fromJust $ decode bytes
  ReqCodeLens           _ -> RspCodeLens $ fromJust $ decode bytes
  ReqCodeLensResolve    _ -> RspCodeLensResolve $ fromJust $ decode bytes
  ReqDocumentFormatting _ -> RspDocumentFormatting $ fromJust $ decode bytes
  ReqDocumentRangeFormatting _ ->
    RspDocumentRangeFormatting $ fromJust $ decode bytes
  ReqDocumentOnTypeFormatting _ ->
    RspDocumentOnTypeFormatting $ fromJust $ decode bytes
  ReqRename              _ -> RspRename $ fromJust $ decode bytes
  ReqExecuteCommand      _ -> RspExecuteCommand $ fromJust $ decode bytes
  ReqDocumentLink        _ -> RspDocumentLink $ fromJust $ decode bytes
  ReqDocumentLinkResolve _ -> RspDocumentLinkResolve $ fromJust $ decode bytes
  ReqWillSaveWaitUntil   _ -> RspWillSaveWaitUntil $ fromJust $ decode bytes
  x                        -> error $ "Not a request: " ++ show x

decodeFromServerMsg :: RequestMap -> B.ByteString -> FromServerMessage
decodeFromServerMsg reqMap bytes =
  case HM.lookup "method" fromJust (decode bytes) of
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
