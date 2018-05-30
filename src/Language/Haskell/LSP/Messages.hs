{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.LSP.Messages
  ( module Language.Haskell.LSP.TH.Messages
  , FromClientMessage(..)
  , FromServerMessage(..)
  )
where

import           Language.Haskell.LSP.TH.Messages
import           Language.Haskell.LSP.TH.DataTypesJSON
import           GHC.Generics
import           Data.Aeson

data FromClientMessage = ReqInitialized              InitializeRequest
                       | ReqShutdown                 ShutdownRequest
                       | ReqHover                    HoverRequest
                       | ReqCompletion               CompletionRequest
                       | ReqCompletionItemResolve    CompletionItemResolveRequest
                       | ReqSignatureHelp            SignatureHelpRequest
                       | ReqDefinition               DefinitionRequest
                       | ReqFindReferences           ReferencesRequest
                       | ReqDocumentHighlights       DocumentHighlightRequest
                       | ReqDocumentSymbols          DocumentSymbolRequest
                       | ReqWorkspaceSymbols         WorkspaceSymbolRequest
                       | ReqCodeAction               CodeActionRequest
                       | ReqCodeLens                 CodeLensRequest
                       | ReqCodeLensResolve          CodeLensResolveRequest
                       | ReqDocumentFormatting       DocumentFormattingRequest
                       | ReqDocumentRangeFormatting  DocumentRangeFormattingRequest
                       | ReqDocumentOnTypeFormatting DocumentOnTypeFormattingRequest
                       | ReqRename                   RenameRequest
                       | ReqExecuteCommand           ExecuteCommandRequest
                       | ReqDocumentLink             DocumentLinkRequest
                       | ReqDocumentLinkResolve      DocumentLinkResolveRequest
                       | ReqWillSaveWaitUntil        WillSaveWaitUntilTextDocumentRequest
                       -- Responses
                       | RspApplyWorkspaceEdit       ApplyWorkspaceEditResponse
                       -- Notifications
                       | NotInitialized                  InitializedNotification
                       | NotExit                         ExitNotification
                       -- A cancel request notification is duplex!
                       | NotCancelRequestFromClient      CancelNotification
                       | NotDidChangeConfiguration       DidChangeConfigurationNotification
                       | NotDidOpenTextDocument          DidOpenTextDocumentNotification
                       | NotDidChangeTextDocument        DidChangeTextDocumentNotification
                       | NotDidCloseTextDocument         DidCloseTextDocumentNotification
                       | NotWillSaveTextDocument         WillSaveTextDocumentNotification
                       | NotDidSaveTextDocument          DidSaveTextDocumentNotification
                       | NotDidChangeWatchedFiles        DidChangeWatchedFilesNotification
                       -- Unknown (The client sends something we don't understand)
                       | UnknownFromClientMessage        Value


  deriving (Eq,Read,Show,Generic,ToJSON,FromJSON)

data FromServerMessage = ReqRegisterCapability       RegisterCapabilityRequest
                       | ReqApplyWorkspaceEdit       ApplyWorkspaceEditRequest
                       | ReqShowMessage              ShowMessageRequest
                       -- Responses
                       | RspInitialize               InitializeResponse
                       | RspShutdown                 ShutdownResponse
                       | RspHover                    HoverResponse
                       | RspCompletion               CompletionResponse
                       | RspCompletionItemResolve    CompletionItemResolveResponse
                       | RspSignatureHelp            SignatureHelpResponse
                       | RspDefinition               DefinitionResponse
                       | RspFindReferences           ReferencesResponse
                       | RspDocumentHighlights       DocumentHighlightsResponse
                       | RspDocumentSymbols          DocumentSymbolsResponse
                       | RspWorkspaceSymbols         WorkspaceSymbolsResponse
                       | RspCodeAction               CodeActionResponse
                       | RspCodeLens                 CodeLensResponse
                       | RspCodeLensResolve          CodeLensResolveResponse
                       | RspDocumentFormatting       DocumentFormattingResponse
                       | RspDocumentRangeFormatting  DocumentRangeFormattingResponse
                       | RspDocumentOnTypeFormatting DocumentOnTypeFormattingResponse
                       | RspRename                   RenameResponse
                       | RspExecuteCommand           ExecuteCommandResponse
                       | RspError                    ErrorResponse
                       | RspDocumentLink             DocumentLinkResponse
                       | RspDocumentLinkResolve      DocumentLinkResolveRequest
                       | RspWillSaveWaitUntil        WillSaveWaitUntilTextDocumentResponse
                       -- Notifications
                       | NotPublishDiagnostics       PublishDiagnosticsNotification
                       | NotLogMessage               LogMessageNotification
                       | NotShowMessage              ShowMessageNotification
                       -- A cancel request notification is duplex!
                       | NotCancelRequestFromServer  CancelNotification
  deriving (Eq,Read,Show,Generic,ToJSON,FromJSON)
