{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SetTraceParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.TraceValue
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data SetTraceParams = SetTraceParams 
  { {-|

  -}
  value :: Language.LSP.Protocol.Internal.Types.TraceValue.TraceValue
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SetTraceParams)

instance Aeson.ToJSON SetTraceParams where
  toJSON (SetTraceParams arg0) = Aeson.object $ concat $  [["value" Aeson..= arg0]]

instance Aeson.FromJSON SetTraceParams where
  parseJSON = Aeson.withObject "SetTraceParams" $ \arg -> SetTraceParams <$> arg Aeson..: "value"
