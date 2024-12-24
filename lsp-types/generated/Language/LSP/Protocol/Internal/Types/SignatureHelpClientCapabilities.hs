{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SignatureHelpClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ClientSignatureInformationOptions
import qualified Language.LSP.Protocol.Types.Common

{-|
Client Capabilities for a `SignatureHelpRequest`.
-}
data SignatureHelpClientCapabilities = SignatureHelpClientCapabilities 
  { {-|
  Whether signature help supports dynamic registration.
  -}
  dynamicRegistration :: (Maybe Bool)
  , {-|
  The client supports the following `SignatureInformation`
  specific properties.
  -}
  signatureInformation :: (Maybe Language.LSP.Protocol.Internal.Types.ClientSignatureInformationOptions.ClientSignatureInformationOptions)
  , {-|
  The client supports to send additional context information for a
  `textDocument/signatureHelp` request. A client that opts into
  contextSupport will also support the `retriggerCharacters` on
  `SignatureHelpOptions`.

  @since 3.15.0
  -}
  contextSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SignatureHelpClientCapabilities)

instance Aeson.ToJSON SignatureHelpClientCapabilities where
  toJSON (SignatureHelpClientCapabilities arg0 arg1 arg2) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"signatureInformation" Language.LSP.Protocol.Types.Common..=? arg1
    ,"contextSupport" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON SignatureHelpClientCapabilities where
  parseJSON = Aeson.withObject "SignatureHelpClientCapabilities" $ \arg -> SignatureHelpClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "signatureInformation" <*> arg Language.LSP.Protocol.Types.Common..:!? "contextSupport"
