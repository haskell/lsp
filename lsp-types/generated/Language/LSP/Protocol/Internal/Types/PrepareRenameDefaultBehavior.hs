{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.PrepareRenameDefaultBehavior where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data PrepareRenameDefaultBehavior = PrepareRenameDefaultBehavior 
  { {-|

  -}
  _defaultBehavior :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON PrepareRenameDefaultBehavior)

instance Aeson.ToJSON PrepareRenameDefaultBehavior where
  toJSON (PrepareRenameDefaultBehavior arg0) = Aeson.object $ concat $  [["defaultBehavior" Aeson..= arg0]]

instance Aeson.FromJSON PrepareRenameDefaultBehavior where
  parseJSON = Aeson.withObject "PrepareRenameDefaultBehavior" $ \arg -> PrepareRenameDefaultBehavior <$> arg Aeson..: "defaultBehavior"
