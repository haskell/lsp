{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientSignatureParameterInformationOptions where

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
data ClientSignatureParameterInformationOptions = ClientSignatureParameterInformationOptions 
  { {-|
  The client supports processing label offsets instead of a
  simple label string.

  @since 3.14.0
  -}
  _labelOffsetSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientSignatureParameterInformationOptions)

instance Aeson.ToJSON ClientSignatureParameterInformationOptions where
  toJSON (ClientSignatureParameterInformationOptions arg0) = Aeson.object $ concat $  ["labelOffsetSupport" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON ClientSignatureParameterInformationOptions where
  parseJSON = Aeson.withObject "ClientSignatureParameterInformationOptions" $ \arg -> ClientSignatureParameterInformationOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "labelOffsetSupport"
