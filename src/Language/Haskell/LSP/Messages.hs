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
                       | ReqTypeDefinition           TypeDefinitionRequest
                       | ReqImplementation           ImplementationRequest
                       | ReqFindReferences           ReferencesRequest
                       | ReqDocumentHighlights       DocumentHighlightRequest
                       | ReqDocumentSymbols          DocumentSymbolRequest
                       | ReqWorkspaceSymbols         WorkspaceSymbolRequest
                       | ReqCodeAction               CodeActionRequest
                       | ReqCodeLens                 CodeLensRequest
                       | ReqCodeLensResolve          CodeLensResolveRequest
                       | ReqDocumentLink             DocumentLinkRequest
                       | ReqDocumentLinkResolve      DocumentLinkResolveRequest
                       | ReqDocumentColor            DocumentColorRequest
                       | ReqColorPresentation        ColorPresentationRequest
                       | ReqDocumentFormatting       DocumentFormattingRequest
                       | ReqDocumentRangeFormatting  DocumentRangeFormattingRequest
                       | ReqDocumentOnTypeFormatting DocumentOnTypeFormattingRequest
                       | ReqRename                   RenameRequest
                       | ReqFoldingRange             FoldingRangeRequest
                       | ReqExecuteCommand           ExecuteCommandRequest
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
                       | NotDidChangeWorkspaceFolders    DidChangeWorkspaceFoldersNotification
                       | NotProgressCancel               ProgressCancelNotification

                       -- It is common for language servers to add custom message types so these
                       -- three constructors can be used to handle custom request, response or notification
                       -- types.
                       | ReqCustomClient                 CustomClientRequest
                       | NotCustomClient                 CustomClientNotification
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
                       | RspTypeDefinition           TypeDefinitionResponse
                       | RspImplementation           ImplementationResponse
                       | RspFindReferences           ReferencesResponse
                       | RspDocumentHighlights       DocumentHighlightsResponse
                       | RspDocumentSymbols          DocumentSymbolsResponse
                       | RspWorkspaceSymbols         WorkspaceSymbolsResponse
                       | RspCodeAction               CodeActionResponse
                       | RspCodeLens                 CodeLensResponse
                       | RspCodeLensResolve          CodeLensResolveResponse
                       | RspDocumentLink             DocumentLinkResponse
                       | RspDocumentLinkResolve      DocumentLinkResolveResponse
                       | RspDocumentColor            DocumentColorResponse
                       | RspColorPresentation        ColorPresentationResponse
                       | RspDocumentFormatting       DocumentFormattingResponse
                       | RspDocumentRangeFormatting  DocumentRangeFormattingResponse
                       | RspDocumentOnTypeFormatting DocumentOnTypeFormattingResponse
                       | RspRename                   RenameResponse
                       | RspFoldingRange             FoldingRangeResponse
                       | RspExecuteCommand           ExecuteCommandResponse
                       | RspError                    ErrorResponse
                       | RspWillSaveWaitUntil        WillSaveWaitUntilTextDocumentResponse
                       -- Notifications
                       | NotPublishDiagnostics       PublishDiagnosticsNotification
                       | NotLogMessage               LogMessageNotification
                       | NotShowMessage              ShowMessageNotification
                       | NotProgressStart            ProgressStartNotification
                       | NotProgressReport           ProgressReportNotification
                       | NotProgressDone             ProgressDoneNotification
                       | NotTelemetry                TelemetryNotification
                       -- A cancel request notification is duplex!
                       | NotCancelRequestFromServer  CancelNotificationServer

                       -- It is common for language servers to add custom message types so these
                       -- three constructors can be used to handle custom request, response or notification
                       -- types.
                       | ReqCustomServer             CustomServerRequest
                       | RspCustomServer             CustomResponse
                       | NotCustomServer             CustomServerNotification
  deriving (Eq,Read,Show,Generic,ToJSON,FromJSON)
