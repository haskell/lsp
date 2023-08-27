{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DiagnosticClientCapabilities where

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
Client capabilities specific to diagnostic pull requests.

@since 3.17.0
-}
data DiagnosticClientCapabilities = DiagnosticClientCapabilities 
  { {-|
  Whether implementation supports dynamic registration. If this is set to `true`
  the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
  return value for the corresponding server capability as well.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  Whether the clients supports related documents for document diagnostic pulls.
  -}
  _relatedDocumentSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DiagnosticClientCapabilities)

instance Aeson.ToJSON DiagnosticClientCapabilities where
  toJSON (DiagnosticClientCapabilities arg0 arg1) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"relatedDocumentSupport" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON DiagnosticClientCapabilities where
  parseJSON = Aeson.withObject "DiagnosticClientCapabilities" $ \arg -> DiagnosticClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "relatedDocumentSupport"
