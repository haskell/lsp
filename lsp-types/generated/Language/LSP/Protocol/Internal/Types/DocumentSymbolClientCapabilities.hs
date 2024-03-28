{- ORMOLU_DISABLE -}
{- HLINT ignore -}
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
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ClientSymbolKindOptions
import qualified Language.LSP.Protocol.Internal.Types.ClientSymbolTagOptions
import qualified Language.LSP.Protocol.Types.Common

{-|
Client Capabilities for a `DocumentSymbolRequest`.
-}
data DocumentSymbolClientCapabilities = DocumentSymbolClientCapabilities 
  { {-|
  Whether document symbol supports dynamic registration.
  -}
  dynamicRegistration :: (Maybe Bool)
  , {-|
  Specific capabilities for the `SymbolKind` in the
  `textDocument/documentSymbol` request.
  -}
  symbolKind :: (Maybe Language.LSP.Protocol.Internal.Types.ClientSymbolKindOptions.ClientSymbolKindOptions)
  , {-|
  The client supports hierarchical document symbols.
  -}
  hierarchicalDocumentSymbolSupport :: (Maybe Bool)
  , {-|
  The client supports tags on `SymbolInformation`. Tags are supported on
  `DocumentSymbol` if `hierarchicalDocumentSymbolSupport` is set to true.
  Clients supporting tags have to handle unknown tags gracefully.

  @since 3.16.0
  -}
  tagSupport :: (Maybe Language.LSP.Protocol.Internal.Types.ClientSymbolTagOptions.ClientSymbolTagOptions)
  , {-|
  The client supports an additional label presented in the UI when
  registering a document symbol provider.

  @since 3.16.0
  -}
  labelSupport :: (Maybe Bool)
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
  parseJSON = Aeson.withObject "DocumentSymbolClientCapabilities" $ \arg -> DocumentSymbolClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "symbolKind" <*> arg Language.LSP.Protocol.Types.Common..:!? "hierarchicalDocumentSymbolSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "tagSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "labelSupport"
