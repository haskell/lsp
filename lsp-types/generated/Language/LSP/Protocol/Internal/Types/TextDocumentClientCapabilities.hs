{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.CodeActionClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.CodeLensClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.CompletionClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DeclarationClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DefinitionClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentColorClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentFormattingClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentHighlightClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentLinkClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.DocumentSymbolClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.FoldingRangeClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.HoverClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ImplementationClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.InlayHintClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.InlineValueClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.LinkedEditingRangeClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.MonikerClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.PublishDiagnosticsClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ReferenceClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.RenameClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.SelectionRangeClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelpClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentSyncClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.TypeDefinitionClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.TypeHierarchyClientCapabilities
import qualified Language.LSP.Protocol.Types.Common

{-|
Text document specific client capabilities.
-}
data TextDocumentClientCapabilities = TextDocumentClientCapabilities 
  { {-|
  Defines which synchronization capabilities the client supports.
  -}
  synchronization :: (Maybe Language.LSP.Protocol.Internal.Types.TextDocumentSyncClientCapabilities.TextDocumentSyncClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/completion` request.
  -}
  completion :: (Maybe Language.LSP.Protocol.Internal.Types.CompletionClientCapabilities.CompletionClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/hover` request.
  -}
  hover :: (Maybe Language.LSP.Protocol.Internal.Types.HoverClientCapabilities.HoverClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/signatureHelp` request.
  -}
  signatureHelp :: (Maybe Language.LSP.Protocol.Internal.Types.SignatureHelpClientCapabilities.SignatureHelpClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/declaration` request.

  @since 3.14.0
  -}
  declaration :: (Maybe Language.LSP.Protocol.Internal.Types.DeclarationClientCapabilities.DeclarationClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/definition` request.
  -}
  definition :: (Maybe Language.LSP.Protocol.Internal.Types.DefinitionClientCapabilities.DefinitionClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/typeDefinition` request.

  @since 3.6.0
  -}
  typeDefinition :: (Maybe Language.LSP.Protocol.Internal.Types.TypeDefinitionClientCapabilities.TypeDefinitionClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/implementation` request.

  @since 3.6.0
  -}
  implementation :: (Maybe Language.LSP.Protocol.Internal.Types.ImplementationClientCapabilities.ImplementationClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/references` request.
  -}
  references :: (Maybe Language.LSP.Protocol.Internal.Types.ReferenceClientCapabilities.ReferenceClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/documentHighlight` request.
  -}
  documentHighlight :: (Maybe Language.LSP.Protocol.Internal.Types.DocumentHighlightClientCapabilities.DocumentHighlightClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/documentSymbol` request.
  -}
  documentSymbol :: (Maybe Language.LSP.Protocol.Internal.Types.DocumentSymbolClientCapabilities.DocumentSymbolClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/codeAction` request.
  -}
  codeAction :: (Maybe Language.LSP.Protocol.Internal.Types.CodeActionClientCapabilities.CodeActionClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/codeLens` request.
  -}
  codeLens :: (Maybe Language.LSP.Protocol.Internal.Types.CodeLensClientCapabilities.CodeLensClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/documentLink` request.
  -}
  documentLink :: (Maybe Language.LSP.Protocol.Internal.Types.DocumentLinkClientCapabilities.DocumentLinkClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/documentColor` and the
  `textDocument/colorPresentation` request.

  @since 3.6.0
  -}
  colorProvider :: (Maybe Language.LSP.Protocol.Internal.Types.DocumentColorClientCapabilities.DocumentColorClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/formatting` request.
  -}
  formatting :: (Maybe Language.LSP.Protocol.Internal.Types.DocumentFormattingClientCapabilities.DocumentFormattingClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/rangeFormatting` request.
  -}
  rangeFormatting :: (Maybe Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingClientCapabilities.DocumentRangeFormattingClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/onTypeFormatting` request.
  -}
  onTypeFormatting :: (Maybe Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingClientCapabilities.DocumentOnTypeFormattingClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/rename` request.
  -}
  rename :: (Maybe Language.LSP.Protocol.Internal.Types.RenameClientCapabilities.RenameClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/foldingRange` request.

  @since 3.10.0
  -}
  foldingRange :: (Maybe Language.LSP.Protocol.Internal.Types.FoldingRangeClientCapabilities.FoldingRangeClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/selectionRange` request.

  @since 3.15.0
  -}
  selectionRange :: (Maybe Language.LSP.Protocol.Internal.Types.SelectionRangeClientCapabilities.SelectionRangeClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/publishDiagnostics` notification.
  -}
  publishDiagnostics :: (Maybe Language.LSP.Protocol.Internal.Types.PublishDiagnosticsClientCapabilities.PublishDiagnosticsClientCapabilities)
  , {-|
  Capabilities specific to the various call hierarchy requests.

  @since 3.16.0
  -}
  callHierarchy :: (Maybe Language.LSP.Protocol.Internal.Types.CallHierarchyClientCapabilities.CallHierarchyClientCapabilities)
  , {-|
  Capabilities specific to the various semantic token request.

  @since 3.16.0
  -}
  semanticTokens :: (Maybe Language.LSP.Protocol.Internal.Types.SemanticTokensClientCapabilities.SemanticTokensClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/linkedEditingRange` request.

  @since 3.16.0
  -}
  linkedEditingRange :: (Maybe Language.LSP.Protocol.Internal.Types.LinkedEditingRangeClientCapabilities.LinkedEditingRangeClientCapabilities)
  , {-|
  Client capabilities specific to the `textDocument/moniker` request.

  @since 3.16.0
  -}
  moniker :: (Maybe Language.LSP.Protocol.Internal.Types.MonikerClientCapabilities.MonikerClientCapabilities)
  , {-|
  Capabilities specific to the various type hierarchy requests.

  @since 3.17.0
  -}
  typeHierarchy :: (Maybe Language.LSP.Protocol.Internal.Types.TypeHierarchyClientCapabilities.TypeHierarchyClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/inlineValue` request.

  @since 3.17.0
  -}
  inlineValue :: (Maybe Language.LSP.Protocol.Internal.Types.InlineValueClientCapabilities.InlineValueClientCapabilities)
  , {-|
  Capabilities specific to the `textDocument/inlayHint` request.

  @since 3.17.0
  -}
  inlayHint :: (Maybe Language.LSP.Protocol.Internal.Types.InlayHintClientCapabilities.InlayHintClientCapabilities)
  , {-|
  Capabilities specific to the diagnostic pull model.

  @since 3.17.0
  -}
  diagnostic :: (Maybe Language.LSP.Protocol.Internal.Types.DiagnosticClientCapabilities.DiagnosticClientCapabilities)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentClientCapabilities)

instance Aeson.ToJSON TextDocumentClientCapabilities where
  toJSON (TextDocumentClientCapabilities arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20 arg21 arg22 arg23 arg24 arg25 arg26 arg27 arg28 arg29) = Aeson.object $ concat $  ["synchronization" Language.LSP.Protocol.Types.Common..=? arg0
    ,"completion" Language.LSP.Protocol.Types.Common..=? arg1
    ,"hover" Language.LSP.Protocol.Types.Common..=? arg2
    ,"signatureHelp" Language.LSP.Protocol.Types.Common..=? arg3
    ,"declaration" Language.LSP.Protocol.Types.Common..=? arg4
    ,"definition" Language.LSP.Protocol.Types.Common..=? arg5
    ,"typeDefinition" Language.LSP.Protocol.Types.Common..=? arg6
    ,"implementation" Language.LSP.Protocol.Types.Common..=? arg7
    ,"references" Language.LSP.Protocol.Types.Common..=? arg8
    ,"documentHighlight" Language.LSP.Protocol.Types.Common..=? arg9
    ,"documentSymbol" Language.LSP.Protocol.Types.Common..=? arg10
    ,"codeAction" Language.LSP.Protocol.Types.Common..=? arg11
    ,"codeLens" Language.LSP.Protocol.Types.Common..=? arg12
    ,"documentLink" Language.LSP.Protocol.Types.Common..=? arg13
    ,"colorProvider" Language.LSP.Protocol.Types.Common..=? arg14
    ,"formatting" Language.LSP.Protocol.Types.Common..=? arg15
    ,"rangeFormatting" Language.LSP.Protocol.Types.Common..=? arg16
    ,"onTypeFormatting" Language.LSP.Protocol.Types.Common..=? arg17
    ,"rename" Language.LSP.Protocol.Types.Common..=? arg18
    ,"foldingRange" Language.LSP.Protocol.Types.Common..=? arg19
    ,"selectionRange" Language.LSP.Protocol.Types.Common..=? arg20
    ,"publishDiagnostics" Language.LSP.Protocol.Types.Common..=? arg21
    ,"callHierarchy" Language.LSP.Protocol.Types.Common..=? arg22
    ,"semanticTokens" Language.LSP.Protocol.Types.Common..=? arg23
    ,"linkedEditingRange" Language.LSP.Protocol.Types.Common..=? arg24
    ,"moniker" Language.LSP.Protocol.Types.Common..=? arg25
    ,"typeHierarchy" Language.LSP.Protocol.Types.Common..=? arg26
    ,"inlineValue" Language.LSP.Protocol.Types.Common..=? arg27
    ,"inlayHint" Language.LSP.Protocol.Types.Common..=? arg28
    ,"diagnostic" Language.LSP.Protocol.Types.Common..=? arg29]

instance Aeson.FromJSON TextDocumentClientCapabilities where
  parseJSON = Aeson.withObject "TextDocumentClientCapabilities" $ \arg -> TextDocumentClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "synchronization" <*> arg Language.LSP.Protocol.Types.Common..:!? "completion" <*> arg Language.LSP.Protocol.Types.Common..:!? "hover" <*> arg Language.LSP.Protocol.Types.Common..:!? "signatureHelp" <*> arg Language.LSP.Protocol.Types.Common..:!? "declaration" <*> arg Language.LSP.Protocol.Types.Common..:!? "definition" <*> arg Language.LSP.Protocol.Types.Common..:!? "typeDefinition" <*> arg Language.LSP.Protocol.Types.Common..:!? "implementation" <*> arg Language.LSP.Protocol.Types.Common..:!? "references" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentHighlight" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentSymbol" <*> arg Language.LSP.Protocol.Types.Common..:!? "codeAction" <*> arg Language.LSP.Protocol.Types.Common..:!? "codeLens" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentLink" <*> arg Language.LSP.Protocol.Types.Common..:!? "colorProvider" <*> arg Language.LSP.Protocol.Types.Common..:!? "formatting" <*> arg Language.LSP.Protocol.Types.Common..:!? "rangeFormatting" <*> arg Language.LSP.Protocol.Types.Common..:!? "onTypeFormatting" <*> arg Language.LSP.Protocol.Types.Common..:!? "rename" <*> arg Language.LSP.Protocol.Types.Common..:!? "foldingRange" <*> arg Language.LSP.Protocol.Types.Common..:!? "selectionRange" <*> arg Language.LSP.Protocol.Types.Common..:!? "publishDiagnostics" <*> arg Language.LSP.Protocol.Types.Common..:!? "callHierarchy" <*> arg Language.LSP.Protocol.Types.Common..:!? "semanticTokens" <*> arg Language.LSP.Protocol.Types.Common..:!? "linkedEditingRange" <*> arg Language.LSP.Protocol.Types.Common..:!? "moniker" <*> arg Language.LSP.Protocol.Types.Common..:!? "typeHierarchy" <*> arg Language.LSP.Protocol.Types.Common..:!? "inlineValue" <*> arg Language.LSP.Protocol.Types.Common..:!? "inlayHint" <*> arg Language.LSP.Protocol.Types.Common..:!? "diagnostic"
