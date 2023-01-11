-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SetTraceParams where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.TraceValues
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data SetTraceParams = SetTraceParams 
  { {-|

  -}
  _value :: Language.LSP.Protocol.Internal.Types.TraceValues.TraceValues
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON SetTraceParams where
  toJSON (SetTraceParams arg0) = Aeson.object $ concat $  [["value" Aeson..= arg0]]

instance Aeson.FromJSON SetTraceParams where
  parseJSON = Aeson.withObject "SetTraceParams" $ \arg -> SetTraceParams <$> arg Aeson..: "value"