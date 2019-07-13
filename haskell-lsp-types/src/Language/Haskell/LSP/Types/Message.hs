{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Language.Haskell.LSP.Types.Message where

import qualified Data.Aeson                                 as A
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Hashable
import           Data.GADT.Compare
import           Data.GADT.Compare.TH
-- For <= 8.2.2
import           Data.Text                                  (Text)
import           Language.Haskell.LSP.Types.Constants


-- | Id used for a request, Can be either a String or an Int
data LspId = IdInt Int | IdString Text
            deriving (Show,Read,Eq,Ord)

instance A.ToJSON LspId where
  toJSON (IdInt i)    = toJSON i
  toJSON (IdString s) = toJSON s

instance A.FromJSON LspId where
  parseJSON v@(A.Number _) = IdInt <$> parseJSON v
  parseJSON  (A.String  s) = return (IdString s)
  parseJSON _              = mempty

instance Hashable LspId where
  hashWithSalt salt (IdInt i) = hashWithSalt salt i
  hashWithSalt salt (IdString s) = hashWithSalt salt s

-- ---------------------------------------------------------------------

-- | Id used for a response, Can be either a String or an Int, or Null. If a
-- request doesn't provide a result value the receiver of a request still needs
-- to return a response message to conform to the JSON RPC specification. The
-- result property of the ResponseMessage should be set to null in this case to
-- signal a successful request.
data LspIdRsp = IdRspInt Int | IdRspString Text | IdRspNull
            deriving (Show,Read,Eq)

instance A.ToJSON LspIdRsp where
  toJSON (IdRspInt i)    = toJSON i
  toJSON (IdRspString s) = toJSON s
  toJSON IdRspNull       = A.Null

instance A.FromJSON LspIdRsp where
  parseJSON v@(A.Number _) = IdRspInt <$> parseJSON v
  parseJSON  (A.String  s) = return $ IdRspString s
  parseJSON  A.Null        = return IdRspNull
  parseJSON _              = mempty

instance Hashable LspIdRsp where
  hashWithSalt salt (IdRspInt i) = hashWithSalt salt i
  hashWithSalt salt (IdRspString s) = hashWithSalt salt s
  hashWithSalt _ IdRspNull = 0

-- | Converts an LspId to its LspIdRsp counterpart.
responseId :: LspId -> LspIdRsp
responseId (IdInt    i) = IdRspInt i
responseId (IdString s) = IdRspString s

-- | Converts an LspIdRsp to its LspId counterpart.
requestId :: LspIdRsp -> LspId
requestId (IdRspInt    i) = IdInt i
requestId (IdRspString s) = IdString s
requestId IdRspNull       = error "Null response id"

-- ---------------------------------------------------------------------

-- Client Methods
data ClientMethod =
 -- General
   Initialize
 | Initialized
 | Shutdown
 | Exit
 | CancelRequest
 -- Workspace
 | WorkspaceDidChangeWorkspaceFolders
 | WorkspaceDidChangeConfiguration
 | WorkspaceDidChangeWatchedFiles
 | WorkspaceSymbol
 | WorkspaceExecuteCommand
 -- Progress
 | WindowProgressCancel
 -- Document
 | TextDocumentDidOpen
 | TextDocumentDidChange
 | TextDocumentWillSave
 | TextDocumentWillSaveWaitUntil
 | TextDocumentDidSave
 | TextDocumentDidClose
 | TextDocumentCompletion
 | CompletionItemResolve
 | TextDocumentHover
 | TextDocumentSignatureHelp
 | TextDocumentDefinition
 | TextDocumentTypeDefinition
 | TextDocumentImplementation
 | TextDocumentReferences
 | TextDocumentDocumentHighlight
 | TextDocumentDocumentSymbol
 | TextDocumentCodeAction
 | TextDocumentCodeLens
 | CodeLensResolve
 | TextDocumentDocumentLink
 | DocumentLinkResolve
 | TextDocumentDocumentColor
 | TextDocumentColorPresentation
 | TextDocumentFormatting
 | TextDocumentRangeFormatting
 | TextDocumentOnTypeFormatting
 | TextDocumentRename
 | TextDocumentFoldingRange
 -- A custom message type. It is not enforced that this starts with $/.
 | CustomClientMethod
   deriving (Eq,Ord,Read,Show)

data SClientMethod (m :: ClientMethod) where
  SInitialize                         :: SClientMethod Initialize
  SInitialized                        :: SClientMethod Initialized
  SShutdown                           :: SClientMethod Shutdown
  SExit                               :: SClientMethod Exit
  SCancelRequest                      :: SClientMethod CancelRequest
  SWorkspaceDidChangeWorkspaceFolders :: SClientMethod WorkspaceDidChangeWorkspaceFolders
  SWorkspaceDidChangeConfiguration    :: SClientMethod WorkspaceDidChangeConfiguration
  SWorkspaceDidChangeWatchedFiles     :: SClientMethod WorkspaceDidChangeWatchedFiles
  SWorkspaceSymbol                    :: SClientMethod WorkspaceSymbol
  SWorkspaceExecuteCommand            :: SClientMethod WorkspaceExecuteCommand
  SWindowProgressCancel               :: SClientMethod WindowProgressCancel
  STextDocumentDidOpen                :: SClientMethod TextDocumentDidOpen
  STextDocumentDidChange              :: SClientMethod TextDocumentDidChange
  STextDocumentWillSave               :: SClientMethod TextDocumentWillSave
  STextDocumentWillSaveWaitUntil      :: SClientMethod TextDocumentWillSaveWaitUntil
  STextDocumentDidSave                :: SClientMethod TextDocumentDidSave
  STextDocumentDidClose               :: SClientMethod TextDocumentDidClose
  STextDocumentCompletion             :: SClientMethod TextDocumentCompletion
  SCompletionItemResolve              :: SClientMethod CompletionItemResolve
  STextDocumentHover                  :: SClientMethod TextDocumentHover
  STextDocumentSignatureHelp          :: SClientMethod TextDocumentSignatureHelp
  STextDocumentDefinition             :: SClientMethod TextDocumentDefinition
  STextDocumentTypeDefinition         :: SClientMethod TextDocumentTypeDefinition
  STextDocumentImplementation         :: SClientMethod TextDocumentImplementation
  STextDocumentReferences             :: SClientMethod TextDocumentReferences
  STextDocumentDocumentHighlight      :: SClientMethod TextDocumentDocumentHighlight
  STextDocumentDocumentSymbol         :: SClientMethod TextDocumentDocumentSymbol
  STextDocumentCodeAction             :: SClientMethod TextDocumentCodeAction
  STextDocumentCodeLens               :: SClientMethod TextDocumentCodeLens
  SCodeLensResolve                    :: SClientMethod CodeLensResolve
  STextDocumentDocumentLink           :: SClientMethod TextDocumentDocumentLink
  SDocumentLinkResolve                :: SClientMethod DocumentLinkResolve
  STextDocumentDocumentColor          :: SClientMethod TextDocumentDocumentColor
  STextDocumentColorPresentation      :: SClientMethod TextDocumentColorPresentation
  STextDocumentFormatting             :: SClientMethod TextDocumentFormatting
  STextDocumentRangeFormatting        :: SClientMethod TextDocumentRangeFormatting
  STextDocumentOnTypeFormatting       :: SClientMethod TextDocumentOnTypeFormatting
  STextDocumentRename                 :: SClientMethod TextDocumentRename
  STextDocumentFoldingRange           :: SClientMethod TextDocumentFoldingRange
  SCustomClientMethod                 :: Text -> SClientMethod CustomClientMethod

deriving instance Eq   (SClientMethod m)
deriving instance Ord  (SClientMethod m)
deriving instance Show (SClientMethod m)

deriveGEq ''SClientMethod
deriveGCompare ''SClientMethod

data SomeClientMethod = forall m. SomeClientMethod (SClientMethod m)

deriving instance Show SomeClientMethod
instance Eq SomeClientMethod where
  (SomeClientMethod a) == (SomeClientMethod b) = defaultEq a b
instance Ord SomeClientMethod where
  (SomeClientMethod a) `compare` (SomeClientMethod b) = defaultCompare a b

instance A.FromJSON SomeClientMethod where
  -- General
  parseJSON (A.String "initialize")                          = return $ SomeClientMethod SInitialize
  parseJSON (A.String "initialized")                         = return $ SomeClientMethod SInitialized
  parseJSON (A.String "shutdown")                            = return $ SomeClientMethod SShutdown
  parseJSON (A.String "exit")                                = return $ SomeClientMethod SExit
  parseJSON (A.String "$/cancelRequest")                     = return $ SomeClientMethod SCancelRequest
 -- Workspace
  parseJSON (A.String "workspace/didChangeWorkspaceFolders") = return $ SomeClientMethod SWorkspaceDidChangeWorkspaceFolders
  parseJSON (A.String "workspace/didChangeConfiguration")    = return $ SomeClientMethod SWorkspaceDidChangeConfiguration
  parseJSON (A.String "workspace/didChangeWatchedFiles")     = return $ SomeClientMethod SWorkspaceDidChangeWatchedFiles
  parseJSON (A.String "workspace/symbol")                    = return $ SomeClientMethod SWorkspaceSymbol
  parseJSON (A.String "workspace/executeCommand")            = return $ SomeClientMethod SWorkspaceExecuteCommand
 -- Document
  parseJSON (A.String "textDocument/didOpen")                = return $ SomeClientMethod STextDocumentDidOpen
  parseJSON (A.String "textDocument/didChange")              = return $ SomeClientMethod STextDocumentDidChange
  parseJSON (A.String "textDocument/willSave")               = return $ SomeClientMethod STextDocumentWillSave
  parseJSON (A.String "textDocument/willSaveWaitUntil")      = return $ SomeClientMethod STextDocumentWillSaveWaitUntil
  parseJSON (A.String "textDocument/didSave")                = return $ SomeClientMethod STextDocumentDidSave
  parseJSON (A.String "textDocument/didClose")               = return $ SomeClientMethod STextDocumentDidClose
  parseJSON (A.String "textDocument/completion")             = return $ SomeClientMethod STextDocumentCompletion
  parseJSON (A.String "completionItem/resolve")              = return $ SomeClientMethod SCompletionItemResolve
  parseJSON (A.String "textDocument/hover")                  = return $ SomeClientMethod STextDocumentHover
  parseJSON (A.String "textDocument/signatureHelp")          = return $ SomeClientMethod STextDocumentSignatureHelp
  parseJSON (A.String "textDocument/definition")             = return $ SomeClientMethod STextDocumentDefinition
  parseJSON (A.String "textDocument/typeDefinition")         = return $ SomeClientMethod STextDocumentTypeDefinition
  parseJSON (A.String "textDocument/implementation")         = return $ SomeClientMethod STextDocumentImplementation
  parseJSON (A.String "textDocument/references")             = return $ SomeClientMethod STextDocumentReferences
  parseJSON (A.String "textDocument/documentHighlight")      = return $ SomeClientMethod STextDocumentDocumentHighlight
  parseJSON (A.String "textDocument/documentSymbol")         = return $ SomeClientMethod STextDocumentDocumentSymbol
  parseJSON (A.String "textDocument/codeAction")             = return $ SomeClientMethod STextDocumentCodeAction
  parseJSON (A.String "textDocument/codeLens")               = return $ SomeClientMethod STextDocumentCodeLens
  parseJSON (A.String "codeLens/resolve")                    = return $ SomeClientMethod SCodeLensResolve
  parseJSON (A.String "textDocument/documentLink")           = return $ SomeClientMethod STextDocumentDocumentLink
  parseJSON (A.String "documentLink/resolve")                = return $ SomeClientMethod SDocumentLinkResolve
  parseJSON (A.String "textDocument/documentColor")          = return $ SomeClientMethod STextDocumentDocumentColor
  parseJSON (A.String "textDocument/colorPresentation")      = return $ SomeClientMethod STextDocumentColorPresentation
  parseJSON (A.String "textDocument/formatting")             = return $ SomeClientMethod STextDocumentFormatting
  parseJSON (A.String "textDocument/rangeFormatting")        = return $ SomeClientMethod STextDocumentRangeFormatting
  parseJSON (A.String "textDocument/onTypeFormatting")       = return $ SomeClientMethod STextDocumentOnTypeFormatting
  parseJSON (A.String "textDocument/rename")                 = return $ SomeClientMethod STextDocumentRename
  parseJSON (A.String "textDocument/foldingRange")           = return $ SomeClientMethod STextDocumentFoldingRange
  parseJSON (A.String "window/progress/cancel")              = return $ SomeClientMethod SWindowProgressCancel
  parseJSON (A.String x)                                     = return $ SomeClientMethod (SCustomClientMethod x)
  parseJSON _                                                = mempty

instance A.FromJSON (SClientMethod Initialize) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SInitialize -> pure $ SInitialize
      _ -> mempty
instance A.FromJSON (SClientMethod Initialized) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SInitialized -> pure $ SInitialized
      _ -> mempty
instance A.FromJSON (SClientMethod Shutdown) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SShutdown -> pure $ SShutdown
      _ -> mempty
instance A.FromJSON (SClientMethod Exit) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SExit -> pure $ SExit
      _ -> mempty
instance A.FromJSON (SClientMethod CancelRequest) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SCancelRequest -> pure $ SCancelRequest
      _ -> mempty
instance A.FromJSON (SClientMethod WorkspaceDidChangeWorkspaceFolders) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SWorkspaceDidChangeWorkspaceFolders -> pure $ SWorkspaceDidChangeWorkspaceFolders
      _ -> mempty
instance A.FromJSON (SClientMethod WorkspaceDidChangeConfiguration) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SWorkspaceDidChangeConfiguration -> pure $ SWorkspaceDidChangeConfiguration
      _ -> mempty
instance A.FromJSON (SClientMethod WorkspaceDidChangeWatchedFiles) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SWorkspaceDidChangeWatchedFiles -> pure $ SWorkspaceDidChangeWatchedFiles
      _ -> mempty
instance A.FromJSON (SClientMethod WorkspaceSymbol) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SWorkspaceSymbol -> pure $ SWorkspaceSymbol
      _ -> mempty
instance A.FromJSON (SClientMethod WorkspaceExecuteCommand) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SWorkspaceExecuteCommand -> pure $ SWorkspaceExecuteCommand
      _ -> mempty
instance A.FromJSON (SClientMethod WindowProgressCancel) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SWindowProgressCancel -> pure $ SWindowProgressCancel
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentDidOpen) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentDidOpen -> pure $ STextDocumentDidOpen
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentDidChange) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentDidChange -> pure $ STextDocumentDidChange
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentWillSave) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentWillSave -> pure $ STextDocumentWillSave
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentWillSaveWaitUntil) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentWillSaveWaitUntil -> pure $ STextDocumentWillSaveWaitUntil
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentDidSave) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentDidSave -> pure $ STextDocumentDidSave
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentDidClose) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentDidClose -> pure $ STextDocumentDidClose
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentCompletion) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentCompletion -> pure $ STextDocumentCompletion
      _ -> mempty
instance A.FromJSON (SClientMethod CompletionItemResolve) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SCompletionItemResolve -> pure $ SCompletionItemResolve
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentHover) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentHover -> pure $ STextDocumentHover
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentSignatureHelp) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentSignatureHelp -> pure $ STextDocumentSignatureHelp
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentDefinition) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentDefinition -> pure $ STextDocumentDefinition
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentTypeDefinition) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentTypeDefinition -> pure $ STextDocumentTypeDefinition
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentImplementation) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentImplementation -> pure $ STextDocumentImplementation
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentReferences) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentReferences -> pure $ STextDocumentReferences
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentDocumentHighlight) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentDocumentHighlight -> pure $ STextDocumentDocumentHighlight
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentDocumentSymbol) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentDocumentSymbol -> pure $ STextDocumentDocumentSymbol
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentCodeAction) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentCodeAction -> pure $ STextDocumentCodeAction
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentCodeLens) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentCodeLens -> pure $ STextDocumentCodeLens
      _ -> mempty
instance A.FromJSON (SClientMethod CodeLensResolve) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SCodeLensResolve -> pure $ SCodeLensResolve
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentDocumentLink) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentDocumentLink -> pure $ STextDocumentDocumentLink
      _ -> mempty
instance A.FromJSON (SClientMethod DocumentLinkResolve) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod SDocumentLinkResolve -> pure $ SDocumentLinkResolve
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentDocumentColor) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentDocumentColor -> pure $ STextDocumentDocumentColor
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentColorPresentation) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentColorPresentation -> pure $ STextDocumentColorPresentation
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentFormatting) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentFormatting -> pure $ STextDocumentFormatting
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentRangeFormatting) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentRangeFormatting -> pure $ STextDocumentRangeFormatting
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentOnTypeFormatting) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentOnTypeFormatting -> pure $ STextDocumentOnTypeFormatting
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentRename) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentRename -> pure $ STextDocumentRename
      _ -> mempty
instance A.FromJSON (SClientMethod TextDocumentFoldingRange) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod STextDocumentFoldingRange -> pure $ STextDocumentFoldingRange
      _ -> mempty
instance A.FromJSON (SClientMethod CustomClientMethod) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeClientMethod (SCustomClientMethod cm) -> pure $ SCustomClientMethod cm
      _ -> mempty

instance A.ToJSON SomeClientMethod where
  toJSON (SomeClientMethod x) = toJSON x
instance A.ToJSON (SClientMethod m) where
  -- General
  toJSON SInitialize                      = A.String "initialize"
  toJSON SInitialized                     = A.String "initialized"
  toJSON SShutdown                        = A.String "shutdown"
  toJSON SExit                            = A.String "exit"
  toJSON SCancelRequest                   = A.String "$/cancelRequest"
  -- Workspace
  toJSON SWorkspaceDidChangeWorkspaceFolders = A.String "workspace/didChangeWorkspaceFolders"
  toJSON SWorkspaceDidChangeConfiguration = A.String "workspace/didChangeConfiguration"
  toJSON SWorkspaceDidChangeWatchedFiles  = A.String "workspace/didChangeWatchedFiles"
  toJSON SWorkspaceSymbol                 = A.String "workspace/symbol"
  toJSON SWorkspaceExecuteCommand         = A.String "workspace/executeCommand"
  -- Document
  toJSON STextDocumentDidOpen             = A.String "textDocument/didOpen"
  toJSON STextDocumentDidChange           = A.String "textDocument/didChange"
  toJSON STextDocumentWillSave            = A.String "textDocument/willSave"
  toJSON STextDocumentWillSaveWaitUntil   = A.String "textDocument/willSaveWaitUntil"
  toJSON STextDocumentDidSave             = A.String "textDocument/didSave"
  toJSON STextDocumentDidClose            = A.String "textDocument/didClose"
  toJSON STextDocumentCompletion          = A.String "textDocument/completion"
  toJSON SCompletionItemResolve           = A.String "completionItem/resolve"
  toJSON STextDocumentHover               = A.String "textDocument/hover"
  toJSON STextDocumentSignatureHelp       = A.String "textDocument/signatureHelp"
  toJSON STextDocumentReferences          = A.String "textDocument/references"
  toJSON STextDocumentDocumentHighlight   = A.String "textDocument/documentHighlight"
  toJSON STextDocumentDocumentSymbol      = A.String "textDocument/documentSymbol"
  toJSON STextDocumentDefinition          = A.String "textDocument/definition"
  toJSON STextDocumentTypeDefinition      = A.String "textDocument/typeDefinition"
  toJSON STextDocumentImplementation      = A.String "textDocument/implementation"
  toJSON STextDocumentCodeAction          = A.String "textDocument/codeAction"
  toJSON STextDocumentCodeLens            = A.String "textDocument/codeLens"
  toJSON SCodeLensResolve                 = A.String "codeLens/resolve"
  toJSON STextDocumentDocumentColor       = A.String "textDocument/documentColor"
  toJSON STextDocumentColorPresentation   = A.String "textDocument/colorPresentation"
  toJSON STextDocumentFormatting          = A.String "textDocument/formatting"
  toJSON STextDocumentRangeFormatting     = A.String "textDocument/rangeFormatting"
  toJSON STextDocumentOnTypeFormatting    = A.String "textDocument/onTypeFormatting"
  toJSON STextDocumentRename              = A.String "textDocument/rename"
  toJSON STextDocumentFoldingRange        = A.String "textDocument/foldingRange"
  toJSON STextDocumentDocumentLink        = A.String "textDocument/documentLink"
  toJSON SDocumentLinkResolve             = A.String "documentLink/resolve"
  toJSON SWindowProgressCancel            = A.String "window/progress/cancel"
  toJSON (SCustomClientMethod xs)         = A.String xs

data ServerMethod =
  -- Window
    WindowShowMessage
  | WindowShowMessageRequest
  | WindowLogMessage
  | WindowProgressStart
  | WindowProgressReport
  | WindowProgressDone
  | TelemetryEvent
  -- Client
  | ClientRegisterCapability
  | ClientUnregisterCapability
  -- Workspace
  | WorkspaceWorkspaceFolders
  | WorkspaceConfiguration
  | WorkspaceApplyEdit
  -- Document
  | TextDocumentPublishDiagnostics
  -- Cancelling
  | CancelRequestServer
  -- Custom
  | CustomServerMethod
   deriving (Eq,Ord,Read,Show)

data SServerMethod (m :: ServerMethod) where
  SWindowShowMessage :: SServerMethod WindowShowMessage
  SWindowShowMessageRequest :: SServerMethod WindowShowMessageRequest
  SWindowLogMessage :: SServerMethod WindowLogMessage
  SWindowProgressStart :: SServerMethod WindowProgressStart
  SWindowProgressReport :: SServerMethod WindowProgressReport
  SWindowProgressDone :: SServerMethod WindowProgressDone
  STelemetryEvent :: SServerMethod TelemetryEvent
  SClientRegisterCapability :: SServerMethod ClientRegisterCapability
  SClientUnregisterCapability :: SServerMethod ClientUnregisterCapability
  SWorkspaceWorkspaceFolders :: SServerMethod WorkspaceWorkspaceFolders
  SWorkspaceConfiguration :: SServerMethod WorkspaceConfiguration
  SWorkspaceApplyEdit :: SServerMethod WorkspaceApplyEdit
  STextDocumentPublishDiagnostics :: SServerMethod TextDocumentPublishDiagnostics
  SCancelRequestServer :: SServerMethod CancelRequestServer
  SCustomServerMethod :: Text -> SServerMethod CustomServerMethod

deriving instance Eq   (SServerMethod m)
deriving instance Ord  (SServerMethod m)
deriving instance Show (SServerMethod m)

deriveGEq ''SServerMethod
deriveGCompare ''SServerMethod

data SomeServerMethod = forall m. SomeServerMethod (SServerMethod m)

instance Eq SomeServerMethod where
  (SomeServerMethod a) == (SomeServerMethod b) = defaultEq a b
instance Ord SomeServerMethod where
  (SomeServerMethod a) `compare` (SomeServerMethod b) = defaultCompare a b

instance A.FromJSON SomeServerMethod where
  -- Window
  parseJSON (A.String "window/showMessage")              = return $ SomeServerMethod SWindowShowMessage
  parseJSON (A.String "window/showMessageRequest")       = return $ SomeServerMethod SWindowShowMessageRequest
  parseJSON (A.String "window/logMessage")               = return $ SomeServerMethod SWindowLogMessage
  parseJSON (A.String "window/progress/start")           = return $ SomeServerMethod SWindowProgressStart
  parseJSON (A.String "window/progress/report")          = return $ SomeServerMethod SWindowProgressReport
  parseJSON (A.String "window/progress/done")            = return $ SomeServerMethod SWindowProgressDone
  parseJSON (A.String "telemetry/event")                 = return $ SomeServerMethod STelemetryEvent
  -- Client
  parseJSON (A.String "client/registerCapability")       = return $ SomeServerMethod SClientRegisterCapability
  parseJSON (A.String "client/unregisterCapability")     = return $ SomeServerMethod SClientUnregisterCapability
  -- Workspace
  parseJSON (A.String "workspace/workspaceFolders")      = return $ SomeServerMethod SWorkspaceWorkspaceFolders
  parseJSON (A.String "workspace/configuration")         = return $ SomeServerMethod SWorkspaceConfiguration
  parseJSON (A.String "workspace/applyEdit")             = return $ SomeServerMethod SWorkspaceApplyEdit
  -- Document
  parseJSON (A.String "textDocument/publishDiagnostics") = return $ SomeServerMethod STextDocumentPublishDiagnostics
  -- Cancelling
  parseJSON (A.String "$/cancelRequest")                 = return $ SomeServerMethod SCancelRequestServer
  parseJSON (A.String m)                                 = return $ SomeServerMethod (SCustomServerMethod m)
  parseJSON _                                            = mempty

instance A.ToJSON SomeServerMethod where
  toJSON (SomeServerMethod m) = toJSON m
instance A.ToJSON (SServerMethod m) where
  -- Window
  toJSON SWindowShowMessage              = A.String "window/showMessage"
  toJSON SWindowShowMessageRequest       = A.String "window/showMessageRequest"
  toJSON SWindowLogMessage               = A.String "window/logMessage"
  toJSON SWindowProgressStart            = A.String "window/progress/start"
  toJSON SWindowProgressReport           = A.String "window/progress/report"
  toJSON SWindowProgressDone             = A.String "window/progress/done"
  toJSON STelemetryEvent                 = A.String "telemetry/event"
  -- Client
  toJSON SClientRegisterCapability       = A.String "client/registerCapability"
  toJSON SClientUnregisterCapability     = A.String "client/unregisterCapability"
  -- Workspace
  toJSON SWorkspaceWorkspaceFolders      = A.String "workspace/workspaceFolders"
  toJSON SWorkspaceConfiguration         = A.String "workspace/configuration"
  toJSON SWorkspaceApplyEdit             = A.String "workspace/applyEdit"
  -- Document
  toJSON STextDocumentPublishDiagnostics = A.String "textDocument/publishDiagnostics"
  -- Cancelling
  toJSON SCancelRequestServer            = A.String "$/cancelRequest"
  toJSON (SCustomServerMethod m)         = A.String m

instance A.FromJSON (SServerMethod WindowShowMessage) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SWindowShowMessage -> pure SWindowShowMessage
      _ -> mempty
instance A.FromJSON (SServerMethod WindowShowMessageRequest) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SWindowShowMessageRequest -> pure SWindowShowMessageRequest
      _ -> mempty
instance A.FromJSON (SServerMethod WindowLogMessage) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SWindowLogMessage -> pure SWindowLogMessage
      _ -> mempty
instance A.FromJSON (SServerMethod WindowProgressStart) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SWindowProgressStart -> pure SWindowProgressStart
      _ -> mempty
instance A.FromJSON (SServerMethod WindowProgressReport) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SWindowProgressReport -> pure SWindowProgressReport
      _ -> mempty
instance A.FromJSON (SServerMethod WindowProgressDone) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SWindowProgressDone -> pure SWindowProgressDone
      _ -> mempty
instance A.FromJSON (SServerMethod TelemetryEvent) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod STelemetryEvent -> pure STelemetryEvent
      _ -> mempty
instance A.FromJSON (SServerMethod ClientRegisterCapability) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SClientRegisterCapability -> pure SClientRegisterCapability
      _ -> mempty
instance A.FromJSON (SServerMethod ClientUnregisterCapability) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SClientUnregisterCapability -> pure SClientUnregisterCapability
      _ -> mempty
instance A.FromJSON (SServerMethod WorkspaceWorkspaceFolders) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SWorkspaceWorkspaceFolders -> pure SWorkspaceWorkspaceFolders
      _ -> mempty
instance A.FromJSON (SServerMethod WorkspaceConfiguration) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SWorkspaceConfiguration -> pure SWorkspaceConfiguration
      _ -> mempty
instance A.FromJSON (SServerMethod WorkspaceApplyEdit) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SWorkspaceApplyEdit -> pure SWorkspaceApplyEdit
      _ -> mempty
instance A.FromJSON (SServerMethod TextDocumentPublishDiagnostics) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod STextDocumentPublishDiagnostics -> pure STextDocumentPublishDiagnostics
      _ -> mempty
instance A.FromJSON (SServerMethod CancelRequestServer) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod SCancelRequestServer -> pure SCancelRequestServer
      _ -> mempty
instance A.FromJSON (SServerMethod CustomServerMethod) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeServerMethod (SCustomServerMethod cm) -> pure (SCustomServerMethod cm)
      _ -> mempty

data RequestMessage m req resp =
  RequestMessage
    { _jsonrpc :: Text
    , _id      :: LspId
    , _method  :: m
    , _params  :: req
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''RequestMessage

-- ---------------------------------------------------------------------
{-
interface ResponseError<D> {
    /**
     * A number indicating the error type that occurred.
     */
    code: number;

    /**
     * A string providing a short description of the error.
     */
    message: string;

    /**
     * A Primitive or Structured value that contains additional
     * information about the error. Can be omitted.
     */
    data?: D;
}

export namespace ErrorCodes {
        // Defined by JSON RPC
        export const ParseError: number = -32700;
        export const InvalidRequest: number = -32600;
        export const MethodNotFound: number = -32601;
        export const InvalidParams: number = -32602;
        export const InternalError: number = -32603;
        export const serverErrorStart: number = -32099;
        export const serverErrorEnd: number = -32000;
        export const ServerNotInitialized: number = -32002;
        export const UnknownErrorCode: number = -32001;

        // Defined by the protocol.
        export const RequestCancelled: number = -32800;
}
-}

data ErrorCode = ParseError
               | InvalidRequest
               | MethodNotFound
               | InvalidParams
               | InternalError
               | ServerErrorStart
               | ServerErrorEnd
               | ServerNotInitialized
               | UnknownErrorCode
               | RequestCancelled
               -- ^ Note: server error codes are reserved from -32099 to -32000
               deriving (Read,Show,Eq)

instance A.ToJSON ErrorCode where
  toJSON ParseError           = A.Number (-32700)
  toJSON InvalidRequest       = A.Number (-32600)
  toJSON MethodNotFound       = A.Number (-32601)
  toJSON InvalidParams        = A.Number (-32602)
  toJSON InternalError        = A.Number (-32603)
  toJSON ServerErrorStart     = A.Number (-32099)
  toJSON ServerErrorEnd       = A.Number (-32000)
  toJSON ServerNotInitialized = A.Number (-32002)
  toJSON UnknownErrorCode     = A.Number (-32001)
  toJSON RequestCancelled     = A.Number (-32800)

instance A.FromJSON ErrorCode where
  parseJSON (A.Number (-32700)) = pure ParseError
  parseJSON (A.Number (-32600)) = pure InvalidRequest
  parseJSON (A.Number (-32601)) = pure MethodNotFound
  parseJSON (A.Number (-32602)) = pure InvalidParams
  parseJSON (A.Number (-32603)) = pure InternalError
  parseJSON (A.Number (-32099)) = pure ServerErrorStart
  parseJSON (A.Number (-32000)) = pure ServerErrorEnd
  parseJSON (A.Number (-32002)) = pure ServerNotInitialized
  parseJSON (A.Number (-32001)) = pure UnknownErrorCode
  parseJSON (A.Number (-32800)) = pure RequestCancelled
  parseJSON _                   = mempty

-- -------------------------------------

data ResponseError =
  ResponseError
    { _code    :: ErrorCode
    , _message :: Text
    , _xdata   :: Maybe A.Value
    } deriving (Read,Show,Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ResponseError

-- ---------------------------------------------------------------------

data ResponseMessage a =
  ResponseMessage
    { _jsonrpc :: Text
    , _id      :: LspIdRsp
    , _result  :: Maybe a
    , _error   :: Maybe ResponseError
    } deriving (Read,Show,Eq)

deriveToJSON lspOptions ''ResponseMessage

instance FromJSON a => FromJSON (ResponseMessage a) where
  parseJSON = withObject "Response" $ \o ->
    ResponseMessage
      <$> o .: "jsonrpc"
      <*> o .: "id"
      -- It is important to use .:! so that result = null gets decoded as Just Nothing
      <*> o .:! "result"
      <*> o .:! "error"

type ErrorResponse = ResponseMessage ()

-- ---------------------------------------------------------------------

type BareResponseMessage = ResponseMessage A.Value

-- ---------------------------------------------------------------------
{-
$ Notifications and Requests

Notification and requests ids starting with '$/' are messages which are protocol
implementation dependent and might not be implementable in all clients or
servers. For example if the server implementation uses a single threaded
synchronous programming language then there is little a server can do to react
to a '$/cancelRequest'. If a server or client receives notifications or requests
starting with '$/' it is free to ignore them if they are unknown.
-}

data NotificationMessage m a =
  NotificationMessage
    { _jsonrpc :: Text
    , _method  :: m
    , _params  :: a
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''NotificationMessage

-- ---------------------------------------------------------------------

data SomeMessage m = ReqMess (RequestMessage m A.Value A.Value) | NotMess (NotificationMessage m A.Value)
  deriving (Eq,Show)

instance (ToJSON m) => ToJSON (SomeMessage m) where
  toJSON (ReqMess a) = toJSON a
  toJSON (NotMess a) = toJSON a

instance (FromJSON m) => FromJSON (SomeMessage m) where
  parseJSON = withObject "SomeMessage" $ \o -> do
    mid <- o .:? "id"
    case (mid :: Maybe LspId) of
      Just _ -> ReqMess <$> parseJSON (A.Object o)
      _ -> NotMess <$> parseJSON (A.Object o)
