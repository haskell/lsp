-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SignatureHelpClientCapabilities where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.MarkupKind
import qualified Language.LSP.Protocol.Types.Common

{-|
Client Capabilities for a `SignatureHelpRequest`.

-}
data SignatureHelpClientCapabilities = SignatureHelpClientCapabilities 
  { {-|
  Whether signature help supports dynamic registration.

  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  The client supports the following `SignatureInformation`
  specific properties.

  -}
  _signatureInformation :: (Maybe (Row.Rec ("documentationFormat" Row..== (Maybe [Language.LSP.Protocol.Internal.Types.MarkupKind.MarkupKind]) Row..+ ("parameterInformation" Row..== (Maybe (Row.Rec ("labelOffsetSupport" Row..== (Maybe Bool) Row..+ Row.Empty))) Row..+ ("activeParameterSupport" Row..== (Maybe Bool) Row..+ Row.Empty)))))
  , {-|
  The client supports to send additional context information for a
  `textDocument/signatureHelp` request. A client that opts into
  contextSupport will also support the `retriggerCharacters` on
  `SignatureHelpOptions`.

  @since 3.15.0

  -}
  _contextSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON SignatureHelpClientCapabilities where
  toJSON (SignatureHelpClientCapabilities arg0 arg1 arg2) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"signatureInformation" Language.LSP.Protocol.Types.Common..=? arg1
    ,"contextSupport" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON SignatureHelpClientCapabilities where
  parseJSON = Aeson.withObject "SignatureHelpClientCapabilities" $ \arg -> SignatureHelpClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "signatureInformation" <*> arg Aeson..:! "contextSupport"