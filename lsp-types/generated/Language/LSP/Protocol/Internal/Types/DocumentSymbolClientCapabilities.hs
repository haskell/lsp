-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentSymbolClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.SymbolKind
import qualified Language.LSP.Protocol.Internal.Types.SymbolTag
import qualified Language.LSP.Protocol.Types.Common

{-|
Client Capabilities for a `DocumentSymbolRequest`.
-}
data DocumentSymbolClientCapabilities = DocumentSymbolClientCapabilities 
  { {-|
  Whether document symbol supports dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  Specific capabilities for the `SymbolKind` in the
  `textDocument/documentSymbol` request.
  -}
  _symbolKind :: (Maybe (Row.Rec ("valueSet" Row..== (Maybe [Language.LSP.Protocol.Internal.Types.SymbolKind.SymbolKind]) Row..+ Row.Empty)))
  , {-|
  The client supports hierarchical document symbols.
  -}
  _hierarchicalDocumentSymbolSupport :: (Maybe Bool)
  , {-|
  The client supports tags on `SymbolInformation`. Tags are supported on
  `DocumentSymbol` if `hierarchicalDocumentSymbolSupport` is set to true.
  Clients supporting tags have to handle unknown tags gracefully.

  @since 3.16.0
  -}
  _tagSupport :: (Maybe (Row.Rec ("valueSet" Row..== [Language.LSP.Protocol.Internal.Types.SymbolTag.SymbolTag] Row..+ Row.Empty)))
  , {-|
  The client supports an additional label presented in the UI when
  registering a document symbol provider.

  @since 3.16.0
  -}
  _labelSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentSymbolClientCapabilities)

instance Aeson.ToJSON DocumentSymbolClientCapabilities where
  toJSON (DocumentSymbolClientCapabilities arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"symbolKind" Language.LSP.Protocol.Types.Common..=? arg1
    ,"hierarchicalDocumentSymbolSupport" Language.LSP.Protocol.Types.Common..=? arg2
    ,"tagSupport" Language.LSP.Protocol.Types.Common..=? arg3
    ,"labelSupport" Language.LSP.Protocol.Types.Common..=? arg4]

instance Aeson.FromJSON DocumentSymbolClientCapabilities where
  parseJSON = Aeson.withObject "DocumentSymbolClientCapabilities" $ \arg -> DocumentSymbolClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "symbolKind" <*> arg Aeson..:! "hierarchicalDocumentSymbolSupport" <*> arg Aeson..:! "tagSupport" <*> arg Aeson..:! "labelSupport"
