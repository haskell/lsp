-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceSymbol where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Location
import qualified Language.LSP.Protocol.Internal.Types.SymbolKind
import qualified Language.LSP.Protocol.Internal.Types.SymbolTag
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A special workspace symbol that supports locations without a range.

See also SymbolInformation.

@since 3.17.0
-}
data WorkspaceSymbol = WorkspaceSymbol 
  { {-|
  The name of this symbol.
  -}
  _name :: Data.Text.Text
  , {-|
  The kind of this symbol.
  -}
  _kind :: Language.LSP.Protocol.Internal.Types.SymbolKind.SymbolKind
  , {-|
  Tags for this symbol.

  @since 3.16.0
  -}
  _tags :: (Maybe [Language.LSP.Protocol.Internal.Types.SymbolTag.SymbolTag])
  , {-|
  The name of the symbol containing this symbol. This information is for
  user interface purposes (e.g. to render a qualifier in the user interface
  if necessary). It can't be used to re-infer a hierarchy for the document
  symbols.
  -}
  _containerName :: (Maybe Data.Text.Text)
  , {-|
  The location of the symbol. Whether a server is allowed to
  return a location without a range depends on the client
  capability `workspace.symbol.resolveSupport`.

  See SymbolInformation#location for more details.
  -}
  _location :: (Language.LSP.Protocol.Internal.Types.Location.Location Language.LSP.Protocol.Types.Common.|? (Row.Rec ("uri" Row..== Language.LSP.Protocol.Types.Uri.Uri Row..+ Row.Empty)))
  , {-|
  A data entry field that is preserved on a workspace symbol between a
  workspace symbol request and a workspace symbol resolve request.
  -}
  _data_ :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON WorkspaceSymbol where
  toJSON (WorkspaceSymbol arg0 arg1 arg2 arg3 arg4 arg5) = Aeson.object $ concat $  [["name" Aeson..= arg0]
    ,["kind" Aeson..= arg1]
    ,"tags" Language.LSP.Protocol.Types.Common..=? arg2
    ,"containerName" Language.LSP.Protocol.Types.Common..=? arg3
    ,["location" Aeson..= arg4]
    ,"data" Language.LSP.Protocol.Types.Common..=? arg5]

instance Aeson.FromJSON WorkspaceSymbol where
  parseJSON = Aeson.withObject "WorkspaceSymbol" $ \arg -> WorkspaceSymbol <$> arg Aeson..: "name" <*> arg Aeson..: "kind" <*> arg Aeson..:! "tags" <*> arg Aeson..:! "containerName" <*> arg Aeson..: "location" <*> arg Aeson..:! "data"