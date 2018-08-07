{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.LSP.Messages
  ( module Language.Haskell.LSP.Types.MessageFuncs
  , FromClientMessage(..)
  , FromServerMessage(..)
  )
where

import           Language.Haskell.LSP.Types.MessageFuncs
import           Language.Haskell.LSP.Types
import           GHC.Generics
import           Data.Aeson

-- | A wrapper around a message that originates from the client
-- and is sent to the server.
data FromClientMessage = ReqInitialize               InitializeRequest
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
                       -- TODO: Remove this and properly decode the type of responses
                       -- based on the id
                       | RspFromClient               BareResponseMessage
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

-- | A wrapper around a message that originates from the server
-- and is sent to the client.
data FromServerMessage = ReqRegisterCapability       RegisterCapabilityRequest
                       | ReqUnregisterCapability     UnregisterCapabilityRequest
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
                       | RspDocumentLinkResolve      DocumentLinkResolveResponse
                       | RspWillSaveWaitUntil        WillSaveWaitUntilTextDocumentResponse
                       -- Notifications
                       | NotPublishDiagnostics       PublishDiagnosticsNotification
                       | NotLogMessage               LogMessageNotification
                       | NotShowMessage              ShowMessageNotification
                       | NotTelemetry                TelemetryNotification
                       -- A cancel request notification is duplex!
                       | NotCancelRequestFromServer  CancelNotificationServer
  deriving (Eq,Read,Show,Generic,ToJSON,FromJSON)