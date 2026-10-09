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
import qualified Language.LSP.Protocol.Internal.Types.ClientDiagnosticsTagOptions
import qualified Language.LSP.Protocol.Types.Common

{-|
Client capabilities specific to diagnostic pull requests.

@since 3.17.0
-}
data DiagnosticClientCapabilities = DiagnosticClientCapabilities 
  { {-|
  Whether the clients accepts diagnostics with related information.
  -}
  _relatedInformation :: (Maybe Bool)
  , {-|
  Client supports the tag property to provide meta data about a diagnostic.
  Clients supporting tags have to handle unknown tags gracefully.

  @since 3.15.0
  -}
  _tagSupport :: (Maybe Language.LSP.Protocol.Internal.Types.ClientDiagnosticsTagOptions.ClientDiagnosticsTagOptions)
  , {-|
  Client supports a codeDescription property

  @since 3.16.0
  -}
  _codeDescriptionSupport :: (Maybe Bool)
  , {-|
  Whether code action supports the `data` property which is
  preserved between a `textDocument/publishDiagnostics` and
  `textDocument/codeAction` request.

  @since 3.16.0
  -}
  _dataSupport :: (Maybe Bool)
  , {-|
  Whether implementation supports dynamic registration. If this is set to `true`
  the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
  return value for the corresponding server capability as well.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  Whether the clients supports related documents for document diagnostic pulls.
  -}
  _relatedDocumentSupport :: (Maybe Bool)
  , {-|
  Whether the client supports `MarkupContent` in diagnostic messages.

  @since 3.18.0
  -}
  _markupMessageSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DiagnosticClientCapabilities)

instance Aeson.ToJSON DiagnosticClientCapabilities where
  toJSON (DiagnosticClientCapabilities arg0 arg1 arg2 arg3 arg4 arg5 arg6) = Aeson.object $ concat $  ["relatedInformation" Language.LSP.Protocol.Types.Common..=? arg0
    ,"tagSupport" Language.LSP.Protocol.Types.Common..=? arg1
    ,"codeDescriptionSupport" Language.LSP.Protocol.Types.Common..=? arg2
    ,"dataSupport" Language.LSP.Protocol.Types.Common..=? arg3
    ,"dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg4
    ,"relatedDocumentSupport" Language.LSP.Protocol.Types.Common..=? arg5
    ,"markupMessageSupport" Language.LSP.Protocol.Types.Common..=? arg6]

instance Aeson.FromJSON DiagnosticClientCapabilities where
  parseJSON = Aeson.withObject "DiagnosticClientCapabilities" $ \arg -> DiagnosticClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "relatedInformation" <*> arg Language.LSP.Protocol.Types.Common..:!? "tagSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "codeDescriptionSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "dataSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "relatedDocumentSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "markupMessageSupport"
