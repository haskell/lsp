{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentSymbol where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Internal.Types.SymbolKind
import qualified Language.LSP.Protocol.Internal.Types.SymbolTag
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents programming constructs like variables, classes, interfaces etc.
that appear in a document. Document symbols can be hierarchical and they
have two ranges: one that encloses its definition and one that points to
its most interesting range, e.g. the range of an identifier.
-}
data DocumentSymbol = DocumentSymbol 
  { {-|
  The name of this symbol. Will be displayed in the user interface and therefore must not be
  an empty string or a string only consisting of white spaces.
  -}
  name :: Data.Text.Text
  , {-|
  More detail for this symbol, e.g the signature of a function.
  -}
  detail :: (Maybe Data.Text.Text)
  , {-|
  The kind of this symbol.
  -}
  kind :: Language.LSP.Protocol.Internal.Types.SymbolKind.SymbolKind
  , {-|
  Tags for this document symbol.

  @since 3.16.0
  -}
  tags :: (Maybe [Language.LSP.Protocol.Internal.Types.SymbolTag.SymbolTag])
  , {-|
  Indicates if this symbol is deprecated.

  @deprecated Use tags instead
  -}
  deprecated :: (Maybe Bool)
  , {-|
  The range enclosing this symbol not including leading/trailing whitespace but everything else
  like comments. This information is typically used to determine if the clients cursor is
  inside the symbol to reveal in the symbol in the UI.
  -}
  range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The range that should be selected and revealed when this symbol is being picked, e.g the name of a function.
  Must be contained by the `range`.
  -}
  selectionRange :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  Children of this symbol, e.g. properties of a class.
  -}
  children :: (Maybe [Language.LSP.Protocol.Internal.Types.DocumentSymbol.DocumentSymbol])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentSymbol)

instance Aeson.ToJSON DocumentSymbol where
  toJSON (DocumentSymbol arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7) = Aeson.object $ concat $  [["name" Aeson..= arg0]
    ,"detail" Language.LSP.Protocol.Types.Common..=? arg1
    ,["kind" Aeson..= arg2]
    ,"tags" Language.LSP.Protocol.Types.Common..=? arg3
    ,"deprecated" Language.LSP.Protocol.Types.Common..=? arg4
    ,["range" Aeson..= arg5]
    ,["selectionRange" Aeson..= arg6]
    ,"children" Language.LSP.Protocol.Types.Common..=? arg7]

instance Aeson.FromJSON DocumentSymbol where
  parseJSON = Aeson.withObject "DocumentSymbol" $ \arg -> DocumentSymbol <$> arg Aeson..: "name" <*> arg Language.LSP.Protocol.Types.Common..:!? "detail" <*> arg Aeson..: "kind" <*> arg Language.LSP.Protocol.Types.Common..:!? "tags" <*> arg Language.LSP.Protocol.Types.Common..:!? "deprecated" <*> arg Aeson..: "range" <*> arg Aeson..: "selectionRange" <*> arg Language.LSP.Protocol.Types.Common..:!? "children"
