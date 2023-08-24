-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Diagnostic where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.CodeDescription
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticRelatedInformation
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticSeverity
import qualified Language.LSP.Protocol.Internal.Types.DiagnosticTag
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents a diagnostic, such as a compiler error or warning. Diagnostic objects
are only valid in the scope of a resource.
-}
data Diagnostic = Diagnostic 
  { {-|
  The range at which the message applies
  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The diagnostic's severity. Can be omitted. If omitted it is up to the
  client to interpret diagnostics as error, warning, info or hint.
  -}
  _severity :: (Maybe Language.LSP.Protocol.Internal.Types.DiagnosticSeverity.DiagnosticSeverity)
  , {-|
  The diagnostic's code, which usually appear in the user interface.
  -}
  _code :: (Maybe (Language.LSP.Protocol.Types.Common.Int32 Language.LSP.Protocol.Types.Common.|? Data.Text.Text))
  , {-|
  An optional property to describe the error code.
  Requires the code field (above) to be present/not null.

  @since 3.16.0
  -}
  _codeDescription :: (Maybe Language.LSP.Protocol.Internal.Types.CodeDescription.CodeDescription)
  , {-|
  A human-readable string describing the source of this
  diagnostic, e.g. 'typescript' or 'super lint'. It usually
  appears in the user interface.
  -}
  _source :: (Maybe Data.Text.Text)
  , {-|
  The diagnostic's message. It usually appears in the user interface
  -}
  _message :: Data.Text.Text
  , {-|
  Additional metadata about the diagnostic.

  @since 3.15.0
  -}
  _tags :: (Maybe [Language.LSP.Protocol.Internal.Types.DiagnosticTag.DiagnosticTag])
  , {-|
  An array of related diagnostic information, e.g. when symbol-names within
  a scope collide all definitions can be marked via this property.
  -}
  _relatedInformation :: (Maybe [Language.LSP.Protocol.Internal.Types.DiagnosticRelatedInformation.DiagnosticRelatedInformation])
  , {-|
  A data entry field that is preserved between a `textDocument/publishDiagnostics`
  notification and `textDocument/codeAction` request.

  @since 3.16.0
  -}
  _data_ :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON Diagnostic)

instance Aeson.ToJSON Diagnostic where
  toJSON (Diagnostic arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,"severity" Language.LSP.Protocol.Types.Common..=? arg1
    ,"code" Language.LSP.Protocol.Types.Common..=? arg2
    ,"codeDescription" Language.LSP.Protocol.Types.Common..=? arg3
    ,"source" Language.LSP.Protocol.Types.Common..=? arg4
    ,["message" Aeson..= arg5]
    ,"tags" Language.LSP.Protocol.Types.Common..=? arg6
    ,"relatedInformation" Language.LSP.Protocol.Types.Common..=? arg7
    ,"data" Language.LSP.Protocol.Types.Common..=? arg8]

instance Aeson.FromJSON Diagnostic where
  parseJSON = Aeson.withObject "Diagnostic" $ \arg -> Diagnostic <$> arg Aeson..: "range" <*> arg Aeson..:! "severity" <*> arg Aeson..:! "code" <*> arg Aeson..:! "codeDescription" <*> arg Aeson..:! "source" <*> arg Aeson..: "message" <*> arg Aeson..:! "tags" <*> arg Aeson..:! "relatedInformation" <*> arg Aeson..:! "data"
