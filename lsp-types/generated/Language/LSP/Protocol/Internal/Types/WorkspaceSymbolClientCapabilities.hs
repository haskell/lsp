{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceSymbolClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ClientSymbolKindOptions
import qualified Language.LSP.Protocol.Internal.Types.ClientSymbolResolveOptions
import qualified Language.LSP.Protocol.Internal.Types.ClientSymbolTagOptions
import qualified Language.LSP.Protocol.Types.Common

{-|
Client capabilities for a `WorkspaceSymbolRequest`.
-}
data WorkspaceSymbolClientCapabilities = WorkspaceSymbolClientCapabilities 
  { {-|
  Symbol request supports dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
  -}
  _symbolKind :: (Maybe Language.LSP.Protocol.Internal.Types.ClientSymbolKindOptions.ClientSymbolKindOptions)
  , {-|
  The client supports tags on `SymbolInformation`.
  Clients supporting tags have to handle unknown tags gracefully.

  @since 3.16.0
  -}
  _tagSupport :: (Maybe Language.LSP.Protocol.Internal.Types.ClientSymbolTagOptions.ClientSymbolTagOptions)
  , {-|
  The client support partial workspace symbols. The client will send the
  request `workspaceSymbol/resolve` to the server to resolve additional
  properties.

  @since 3.17.0
  -}
  _resolveSupport :: (Maybe Language.LSP.Protocol.Internal.Types.ClientSymbolResolveOptions.ClientSymbolResolveOptions)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkspaceSymbolClientCapabilities)

instance Aeson.ToJSON WorkspaceSymbolClientCapabilities where
  toJSON (WorkspaceSymbolClientCapabilities arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"symbolKind" Language.LSP.Protocol.Types.Common..=? arg1
    ,"tagSupport" Language.LSP.Protocol.Types.Common..=? arg2
    ,"resolveSupport" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON WorkspaceSymbolClientCapabilities where
  parseJSON = Aeson.withObject "WorkspaceSymbolClientCapabilities" $ \arg -> WorkspaceSymbolClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "symbolKind" <*> arg Language.LSP.Protocol.Types.Common..:!? "tagSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "resolveSupport"
