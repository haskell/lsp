{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientDiagnosticsTagOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticTag
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientDiagnosticsTagOptions = ClientDiagnosticsTagOptions 
  { {-|
  The tags supported by the client.
  -}
  valueSet :: [Language.LSP.Protocol.Internal.Types.DiagnosticTag.DiagnosticTag]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientDiagnosticsTagOptions)

instance Aeson.ToJSON ClientDiagnosticsTagOptions where
  toJSON (ClientDiagnosticsTagOptions arg0) = Aeson.object $ concat $  [["valueSet" Aeson..= arg0]]

instance Aeson.FromJSON ClientDiagnosticsTagOptions where
  parseJSON = Aeson.withObject "ClientDiagnosticsTagOptions" $ \arg -> ClientDiagnosticsTagOptions <$> arg Aeson..: "valueSet"
