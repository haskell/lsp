{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidChangeConfigurationRegistrationOptions where

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

{-|

-}
data DidChangeConfigurationRegistrationOptions = DidChangeConfigurationRegistrationOptions 
  { {-|

  -}
  section :: (Maybe (Data.Text.Text Language.LSP.Protocol.Types.Common.|? [Data.Text.Text]))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DidChangeConfigurationRegistrationOptions)

instance Aeson.ToJSON DidChangeConfigurationRegistrationOptions where
  toJSON (DidChangeConfigurationRegistrationOptions arg0) = Aeson.object $ concat $  ["section" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON DidChangeConfigurationRegistrationOptions where
  parseJSON = Aeson.withObject "DidChangeConfigurationRegistrationOptions" $ \arg -> DidChangeConfigurationRegistrationOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "section"
