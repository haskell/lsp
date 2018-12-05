{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Test.Decoding where

import           Prelude                 hiding ( id )
import           Data.Aeson
import           Control.Exception
import           Control.Lens
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Maybe
import           System.IO
import           System.IO.Error
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Lens
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
  l <- catch (hGetLine h) eofHandler 
  let (name, val) = span (/= ':') l
  if null val then return [] else ((name, drop 2 val) :) <$> getHeaders h
  where eofHandler e
          | isEOFError e = error "Language Server unexpectedly terminated"
          | otherwise = throw e

type RequestMap = HM.HashMap LspId ClientMethod

newRequestMap :: RequestMap
newRequestMap = HM.empty

updateRequestMap :: RequestMap -> LspId -> ClientMethod -> RequestMap
updateRequestMap reqMap id method = HM.insert id method reqMap

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
matchResponseMsgType req = case req of
  Initialize                    -> RspInitialize . decoded
  Shutdown                      -> RspShutdown . decoded
  TextDocumentHover             -> RspHover . decoded
  TextDocumentCompletion        -> RspCompletion . decoded
  CompletionItemResolve         -> RspCompletionItemResolve . decoded
  TextDocumentSignatureHelp     -> RspSignatureHelp . decoded
  TextDocumentDefinition        -> RspDefinition . decoded
  TextDocumentReferences        -> RspFindReferences . decoded
  TextDocumentDocumentHighlight -> RspDocumentHighlights . decoded
  TextDocumentDocumentSymbol    -> RspDocumentSymbols . decoded
  WorkspaceSymbol               -> RspWorkspaceSymbols . decoded
  TextDocumentCodeAction        -> RspCodeAction . decoded
  TextDocumentCodeLens          -> RspCodeLens . decoded
  CodeLensResolve               -> RspCodeLensResolve . decoded
  TextDocumentFormatting        -> RspDocumentFormatting . decoded
  TextDocumentRangeFormatting   -> RspDocumentRangeFormatting . decoded
  TextDocumentOnTypeFormatting  -> RspDocumentOnTypeFormatting . decoded
  TextDocumentRename            -> RspRename . decoded
  WorkspaceExecuteCommand       -> RspExecuteCommand . decoded
  TextDocumentDocumentLink      -> RspDocumentLink . decoded
  DocumentLinkResolve           -> RspDocumentLinkResolve . decoded
  TextDocumentWillSaveWaitUntil -> RspWillSaveWaitUntil . decoded
  x                             -> error . ((show x ++ " is not a request: ") ++) . show
  where decoded x = fromMaybe (error $ "Couldn't decode response for the request type: "
                                        ++ show req ++ "\n" ++ show x)
                              (decode x)

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
        WorkspaceWorkspaceFolders      -> error "ReqWorkspaceFolders not supported yet"
        WorkspaceConfiguration         -> error "ReqWorkspaceConfiguration not supported yet"

      Error e -> error e

    Nothing -> case decode bytes :: Maybe (ResponseMessage Value) of
      Just msg -> case HM.lookup (requestId $ msg ^. id) reqMap of
        Just req -> matchResponseMsgType req bytes -- try to decode it to more specific type
        Nothing  -> error "Couldn't match up response with request"
      Nothing -> error "Couldn't decode message"
