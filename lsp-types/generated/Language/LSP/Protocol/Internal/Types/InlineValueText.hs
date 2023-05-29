-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineValueText where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
Provide inline value as text.

@since 3.17.0

-}
data InlineValueText = InlineValueText 
  { {-|
  The document range for which the inline value applies.

  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The text of the inline value.

  -}
  _text :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON InlineValueText where
  toJSON (InlineValueText arg0 arg1) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,["text" Aeson..= arg1]]

instance Aeson.FromJSON InlineValueText where
  parseJSON = Aeson.withObject "InlineValueText" $ \arg -> InlineValueText <$> arg Aeson..: "range" <*> arg Aeson..: "text"