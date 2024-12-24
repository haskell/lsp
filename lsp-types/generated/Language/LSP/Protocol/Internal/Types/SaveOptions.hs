{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SaveOptions where

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
Save options.
-}
data SaveOptions = SaveOptions 
  { {-|
  The client is supposed to include the content on save.
  -}
  includeText :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SaveOptions)

instance Aeson.ToJSON SaveOptions where
  toJSON (SaveOptions arg0) = Aeson.object $ concat $  ["includeText" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON SaveOptions where
  parseJSON = Aeson.withObject "SaveOptions" $ \arg -> SaveOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "includeText"
