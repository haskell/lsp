-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DiagnosticServerCancellationData where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Cancellation data returned from a diagnostic request.

@since 3.17.0
-}
data DiagnosticServerCancellationData = DiagnosticServerCancellationData 
  { {-|

  -}
  _retriggerRequest :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON DiagnosticServerCancellationData where
  toJSON (DiagnosticServerCancellationData arg0) = Aeson.object $ concat $  [["retriggerRequest" Aeson..= arg0]]

instance Aeson.FromJSON DiagnosticServerCancellationData where
  parseJSON = Aeson.withObject "DiagnosticServerCancellationData" $ \arg -> DiagnosticServerCancellationData <$> arg Aeson..: "retriggerRequest"
