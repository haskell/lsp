{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.MarkedStringWithLanguage where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-# DEPRECATED MarkedStringWithLanguage "use MarkupContent instead." #-}
{-|
@since 3.18.0
@proposed
@deprecated use MarkupContent instead.
-}
data MarkedStringWithLanguage = MarkedStringWithLanguage 
  { {-|

  -}
  language :: Data.Text.Text
  , {-|

  -}
  value :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON MarkedStringWithLanguage)

instance Aeson.ToJSON MarkedStringWithLanguage where
  toJSON (MarkedStringWithLanguage arg0 arg1) = Aeson.object $ concat $  [["language" Aeson..= arg0]
    ,["value" Aeson..= arg1]]

instance Aeson.FromJSON MarkedStringWithLanguage where
  parseJSON = Aeson.withObject "MarkedStringWithLanguage" $ \arg -> MarkedStringWithLanguage <$> arg Aeson..: "language" <*> arg Aeson..: "value"
