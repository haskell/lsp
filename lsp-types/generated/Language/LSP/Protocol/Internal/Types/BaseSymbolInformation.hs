-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.BaseSymbolInformation where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.SymbolKind
import qualified Language.LSP.Protocol.Internal.Types.SymbolTag
import qualified Language.LSP.Protocol.Types.Common

{-|
A base for all symbol information.
-}
data BaseSymbolInformation = BaseSymbolInformation 
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
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON BaseSymbolInformation where
  toJSON (BaseSymbolInformation arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["name" Aeson..= arg0]
    ,["kind" Aeson..= arg1]
    ,"tags" Language.LSP.Protocol.Types.Common..=? arg2
    ,"containerName" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON BaseSymbolInformation where
  parseJSON = Aeson.withObject "BaseSymbolInformation" $ \arg -> BaseSymbolInformation <$> arg Aeson..: "name" <*> arg Aeson..: "kind" <*> arg Aeson..:! "tags" <*> arg Aeson..:! "containerName"
