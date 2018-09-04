{-# LANGUAGE RankNTypes #-}
module Language.Haskell.LSP.Test.Messages where

import Data.Aeson
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types hiding (error)

isServerResponse :: FromServerMessage -> Bool
isServerResponse (RspInitialize               _) = True
isServerResponse (RspShutdown                 _) = True
isServerResponse (RspHover                    _) = True
isServerResponse (RspCompletion               _) = True
isServerResponse (RspCompletionItemResolve    _) = True
isServerResponse (RspSignatureHelp            _) = True
isServerResponse (RspDefinition               _) = True
isServerResponse (RspFindReferences           _) = True
isServerResponse (RspDocumentHighlights       _) = True
isServerResponse (RspDocumentSymbols          _) = True
isServerResponse (RspWorkspaceSymbols         _) = True
isServerResponse (RspCodeAction               _) = True
isServerResponse (RspCodeLens                 _) = True
isServerResponse (RspCodeLensResolve          _) = True
isServerResponse (RspDocumentFormatting       _) = True
isServerResponse (RspDocumentRangeFormatting  _) = True
isServerResponse (RspDocumentOnTypeFormatting _) = True
isServerResponse (RspRename                   _) = True
isServerResponse (RspExecuteCommand           _) = True
isServerResponse (RspError                    _) = True
isServerResponse (RspDocumentLink             _) = True
isServerResponse (RspDocumentLinkResolve      _) = True
isServerResponse (RspWillSaveWaitUntil        _) = True
isServerResponse _                               = False

isServerRequest :: FromServerMessage -> Bool
isServerRequest (ReqRegisterCapability       _) = True
isServerRequest (ReqApplyWorkspaceEdit       _) = True
isServerRequest (ReqShowMessage              _) = True
isServerRequest (ReqUnregisterCapability     _) = True
isServerRequest _                               = False

isServerNotification :: FromServerMessage -> Bool
isServerNotification (NotPublishDiagnostics       _) = True
isServerNotification (NotLogMessage               _) = True
isServerNotification (NotShowMessage              _) = True
isServerNotification (NotTelemetry                _) = True
isServerNotification (NotCancelRequestFromServer  _) = True
isServerNotification _                               = False

handleServerMessage
    :: forall a.
       (forall b c. RequestMessage ServerMethod b c -> a)
    -> (forall d. ResponseMessage d -> a)
    -> (forall e. NotificationMessage ServerMethod e -> a)
    -> FromServerMessage
    -> a
handleServerMessage request response notification msg = case msg of
    (ReqRegisterCapability       m) -> request m
    (ReqApplyWorkspaceEdit       m) -> request m
    (ReqShowMessage              m) -> request m
    (ReqUnregisterCapability     m) -> request m
    (RspInitialize               m) -> response m
    (RspShutdown                 m) -> response m
    (RspHover                    m) -> response m
    (RspCompletion               m) -> response m
    (RspCompletionItemResolve    m) -> response m
    (RspSignatureHelp            m) -> response m
    (RspDefinition               m) -> response m
    (RspFindReferences           m) -> response m
    (RspDocumentHighlights       m) -> response m
    (RspDocumentSymbols          m) -> response m
    (RspWorkspaceSymbols         m) -> response m
    (RspCodeAction               m) -> response m
    (RspCodeLens                 m) -> response m
    (RspCodeLensResolve          m) -> response m
    (RspDocumentFormatting       m) -> response m
    (RspDocumentRangeFormatting  m) -> response m
    (RspDocumentOnTypeFormatting m) -> response m
    (RspRename                   m) -> response m
    (RspExecuteCommand           m) -> response m
    (RspError                    m) -> response m
    (RspDocumentLink             m) -> response m
    (RspDocumentLinkResolve      m) -> response m
    (RspWillSaveWaitUntil        m) -> response m
    (RspTypeDefinition           m) -> response m
    (RspImplementation           m) -> response m
    (RspDocumentColor            m) -> response m
    (RspColorPresentation        m) -> response m
    (RspFoldingRange             m) -> response m
    (NotPublishDiagnostics       m) -> notification m
    (NotLogMessage               m) -> notification m
    (NotShowMessage              m) -> notification m
    (NotTelemetry                m) -> notification m
    (NotCancelRequestFromServer  m) -> notification m

handleClientMessage
    :: forall a.
       (forall b c . (ToJSON b, ToJSON c) => RequestMessage ClientMethod b c -> a)
    -> (forall d . ToJSON d => ResponseMessage d -> a)
    -> (forall e . ToJSON e => NotificationMessage ClientMethod e -> a)
    -> FromClientMessage
    -> a
handleClientMessage request response notification msg = case msg of
 (ReqInitialize               m) -> request m
 (ReqShutdown                 m) -> request m
 (ReqHover                    m) -> request m
 (ReqCompletion               m) -> request m
 (ReqCompletionItemResolve    m) -> request m
 (ReqSignatureHelp            m) -> request m
 (ReqDefinition               m) -> request m
 (ReqFindReferences           m) -> request m
 (ReqDocumentHighlights       m) -> request m
 (ReqDocumentSymbols          m) -> request m
 (ReqWorkspaceSymbols         m) -> request m
 (ReqCodeAction               m) -> request m
 (ReqCodeLens                 m) -> request m
 (ReqCodeLensResolve          m) -> request m
 (ReqDocumentFormatting       m) -> request m
 (ReqDocumentRangeFormatting  m) -> request m
 (ReqDocumentOnTypeFormatting m) -> request m
 (ReqRename                   m) -> request m
 (ReqExecuteCommand           m) -> request m
 (ReqDocumentLink             m) -> request m
 (ReqDocumentLinkResolve      m) -> request m
 (ReqWillSaveWaitUntil        m) -> request m
 (ReqImplementation           m) -> request m
 (ReqTypeDefinition           m) -> request m
 (ReqDocumentColor            m) -> request m
 (ReqColorPresentation        m) -> request m
 (ReqFoldingRange             m) -> request m
 (RspApplyWorkspaceEdit       m) -> response m
 (RspFromClient               m) -> response m
 (NotInitialized              m) -> notification m
 (NotExit                     m) -> notification m
 (NotCancelRequestFromClient  m) -> notification m
 (NotDidChangeConfiguration   m) -> notification m
 (NotDidOpenTextDocument      m) -> notification m
 (NotDidChangeTextDocument    m) -> notification m
 (NotDidCloseTextDocument     m) -> notification m
 (NotWillSaveTextDocument     m) -> notification m
 (NotDidSaveTextDocument      m) -> notification m
 (NotDidChangeWatchedFiles    m) -> notification m
 (NotDidChangeWorkspaceFolders m) -> notification m
 (UnknownFromClientMessage    m) -> error $ "Unknown message sent from client: " ++ show m
