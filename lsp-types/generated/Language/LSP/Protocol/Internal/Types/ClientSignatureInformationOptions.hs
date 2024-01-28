{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientSignatureInformationOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ClientSignatureParameterInformationOptions
import qualified Language.LSP.Protocol.Internal.Types.MarkupKind
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientSignatureInformationOptions = ClientSignatureInformationOptions 
  { {-|
  Client supports the following content formats for the documentation
  property. The order describes the preferred format of the client.
  -}
  _documentationFormat :: (Maybe [Language.LSP.Protocol.Internal.Types.MarkupKind.MarkupKind])
  , {-|
  Client capabilities specific to parameter information.
  -}
  _parameterInformation :: (Maybe Language.LSP.Protocol.Internal.Types.ClientSignatureParameterInformationOptions.ClientSignatureParameterInformationOptions)
  , {-|
  The client supports the `activeParameter` property on `SignatureInformation`
  literal.

  @since 3.16.0
  -}
  _activeParameterSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientSignatureInformationOptions)

instance Aeson.ToJSON ClientSignatureInformationOptions where
  toJSON (ClientSignatureInformationOptions arg0 arg1 arg2) = Aeson.object $ concat $  ["documentationFormat" Language.LSP.Protocol.Types.Common..=? arg0
    ,"parameterInformation" Language.LSP.Protocol.Types.Common..=? arg1
    ,"activeParameterSupport" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON ClientSignatureInformationOptions where
  parseJSON = Aeson.withObject "ClientSignatureInformationOptions" $ \arg -> ClientSignatureInformationOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "documentationFormat" <*> arg Language.LSP.Protocol.Types.Common..:!? "parameterInformation" <*> arg Language.LSP.Protocol.Types.Common..:!? "activeParameterSupport"
