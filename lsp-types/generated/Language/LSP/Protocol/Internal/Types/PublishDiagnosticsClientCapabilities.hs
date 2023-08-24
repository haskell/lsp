{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.PublishDiagnosticsClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticTag
import qualified Language.LSP.Protocol.Types.Common

{-|
The publish diagnostic client capabilities.
-}
data PublishDiagnosticsClientCapabilities = PublishDiagnosticsClientCapabilities 
  { {-|
  Whether the clients accepts diagnostics with related information.
  -}
  _relatedInformation :: (Maybe Bool)
  , {-|
  Client supports the tag property to provide meta data about a diagnostic.
  Clients supporting tags have to handle unknown tags gracefully.

  @since 3.15.0
  -}
  _tagSupport :: (Maybe (Row.Rec ("valueSet" Row..== [Language.LSP.Protocol.Internal.Types.DiagnosticTag.DiagnosticTag] Row..+ Row.Empty)))
  , {-|
  Whether the client interprets the version property of the
  `textDocument/publishDiagnostics` notification's parameter.

  @since 3.15.0
  -}
  _versionSupport :: (Maybe Bool)
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
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON PublishDiagnosticsClientCapabilities)

instance Aeson.ToJSON PublishDiagnosticsClientCapabilities where
  toJSON (PublishDiagnosticsClientCapabilities arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  ["relatedInformation" Language.LSP.Protocol.Types.Common..=? arg0
    ,"tagSupport" Language.LSP.Protocol.Types.Common..=? arg1
    ,"versionSupport" Language.LSP.Protocol.Types.Common..=? arg2
    ,"codeDescriptionSupport" Language.LSP.Protocol.Types.Common..=? arg3
    ,"dataSupport" Language.LSP.Protocol.Types.Common..=? arg4]

instance Aeson.FromJSON PublishDiagnosticsClientCapabilities where
  parseJSON = Aeson.withObject "PublishDiagnosticsClientCapabilities" $ \arg -> PublishDiagnosticsClientCapabilities <$> arg Aeson..:! "relatedInformation" <*> arg Aeson..:! "tagSupport" <*> arg Aeson..:! "versionSupport" <*> arg Aeson..:! "codeDescriptionSupport" <*> arg Aeson..:! "dataSupport"
