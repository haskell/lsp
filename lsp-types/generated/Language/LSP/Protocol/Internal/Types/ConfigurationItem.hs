{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ConfigurationItem where

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
import qualified Language.LSP.Protocol.Types.Uri

{-|

-}
data ConfigurationItem = ConfigurationItem 
  { {-|
  The scope to get the configuration section for.
  -}
  _scopeUri :: (Maybe Language.LSP.Protocol.Types.Uri.Uri)
  , {-|
  The configuration section asked for.
  -}
  _section :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ConfigurationItem)

instance Aeson.ToJSON ConfigurationItem where
  toJSON (ConfigurationItem arg0 arg1) = Aeson.object $ concat $  ["scopeUri" Language.LSP.Protocol.Types.Common..=? arg0
    ,"section" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON ConfigurationItem where
  parseJSON = Aeson.withObject "ConfigurationItem" $ \arg -> ConfigurationItem <$> arg Language.LSP.Protocol.Types.Common..:!? "scopeUri" <*> arg Language.LSP.Protocol.Types.Common..:!? "section"
