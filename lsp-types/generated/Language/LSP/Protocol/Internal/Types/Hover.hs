-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Hover where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.MarkedString
import qualified Language.LSP.Protocol.Internal.Types.MarkupContent
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
The result of a hover request.

-}
data Hover = Hover 
  { {-|
  The hover's content

  -}
  _contents :: (Language.LSP.Protocol.Internal.Types.MarkupContent.MarkupContent Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.MarkedString.MarkedString Language.LSP.Protocol.Types.Common.|? [Language.LSP.Protocol.Internal.Types.MarkedString.MarkedString]))
  , {-|
  An optional range inside the text document that is used to
  visualize the hover, e.g. by changing the background color.

  -}
  _range :: (Maybe Language.LSP.Protocol.Internal.Types.Range.Range)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON Hover where
  toJSON (Hover arg0 arg1) = Aeson.object $ concat $  [["contents" Aeson..= arg0]
    ,"range" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON Hover where
  parseJSON = Aeson.withObject "Hover" $ \arg -> Hover <$> arg Aeson..: "contents" <*> arg Aeson..:! "range"