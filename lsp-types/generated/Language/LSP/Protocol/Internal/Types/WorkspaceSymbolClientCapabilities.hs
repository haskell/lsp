-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceSymbolClientCapabilities where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.SymbolKind
import qualified Language.LSP.Protocol.Internal.Types.SymbolTag
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
  _symbolKind :: (Maybe (Row.Rec ("valueSet" Row..== (Maybe [Language.LSP.Protocol.Internal.Types.SymbolKind.SymbolKind]) Row..+ Row.Empty)))
  , {-|
  The client supports tags on `SymbolInformation`.
  Clients supporting tags have to handle unknown tags gracefully.

  @since 3.16.0
  -}
  _tagSupport :: (Maybe (Row.Rec ("valueSet" Row..== [Language.LSP.Protocol.Internal.Types.SymbolTag.SymbolTag] Row..+ Row.Empty)))
  , {-|
  The client support partial workspace symbols. The client will send the
  request `workspaceSymbol/resolve` to the server to resolve additional
  properties.

  @since 3.17.0
  -}
  _resolveSupport :: (Maybe (Row.Rec ("properties" Row..== [Data.Text.Text] Row..+ Row.Empty)))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON WorkspaceSymbolClientCapabilities where
  toJSON (WorkspaceSymbolClientCapabilities arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"symbolKind" Language.LSP.Protocol.Types.Common..=? arg1
    ,"tagSupport" Language.LSP.Protocol.Types.Common..=? arg2
    ,"resolveSupport" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON WorkspaceSymbolClientCapabilities where
  parseJSON = Aeson.withObject "WorkspaceSymbolClientCapabilities" $ \arg -> WorkspaceSymbolClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "symbolKind" <*> arg Aeson..:! "tagSupport" <*> arg Aeson..:! "resolveSupport"