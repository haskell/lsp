-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SymbolInformation where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Location
import qualified Language.LSP.Protocol.Internal.Types.SymbolKind
import qualified Language.LSP.Protocol.Internal.Types.SymbolTag
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents information about programming constructs like variables, classes,
interfaces etc.
-}
data SymbolInformation = SymbolInformation 
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
  Indicates if this symbol is deprecated.

  @deprecated Use tags instead
  -}
  _deprecated :: (Maybe Bool)
  , {-|
  The location of this symbol. The location's range is used by a tool
  to reveal the location in the editor. If the symbol is selected in the
  tool the range's start information is used to position the cursor. So
  the range usually spans more than the actual symbol's name and does
  normally include things like visibility modifiers.

  The range doesn't have to denote a node range in the sense of an abstract
  syntax tree. It can therefore not be used to re-construct a hierarchy of
  the symbols.
  -}
  _location :: Language.LSP.Protocol.Internal.Types.Location.Location
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SymbolInformation)

instance Aeson.ToJSON SymbolInformation where
  toJSON (SymbolInformation arg0 arg1 arg2 arg3 arg4 arg5) = Aeson.object $ concat $  [["name" Aeson..= arg0]
    ,["kind" Aeson..= arg1]
    ,"tags" Language.LSP.Protocol.Types.Common..=? arg2
    ,"containerName" Language.LSP.Protocol.Types.Common..=? arg3
    ,"deprecated" Language.LSP.Protocol.Types.Common..=? arg4
    ,["location" Aeson..= arg5]]

instance Aeson.FromJSON SymbolInformation where
  parseJSON = Aeson.withObject "SymbolInformation" $ \arg -> SymbolInformation <$> arg Aeson..: "name" <*> arg Aeson..: "kind" <*> arg Aeson..:! "tags" <*> arg Aeson..:! "containerName" <*> arg Aeson..:! "deprecated" <*> arg Aeson..: "location"
