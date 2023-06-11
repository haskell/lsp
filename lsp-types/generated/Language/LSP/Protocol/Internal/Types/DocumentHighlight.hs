-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentHighlight where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.DocumentHighlightKind
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
A document highlight is a range inside a text document which deserves
special attention. Usually a document highlight is visualized by changing
the background color of its range.
-}
data DocumentHighlight = DocumentHighlight 
  { {-|
  The range this highlight applies to.
  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The highlight kind, default is `DocumentHighlightKind.Text`.
  -}
  _kind :: (Maybe Language.LSP.Protocol.Internal.Types.DocumentHighlightKind.DocumentHighlightKind)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON DocumentHighlight where
  toJSON (DocumentHighlight arg0 arg1) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,"kind" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON DocumentHighlight where
  parseJSON = Aeson.withObject "DocumentHighlight" $ \arg -> DocumentHighlight <$> arg Aeson..: "range" <*> arg Aeson..:! "kind"
