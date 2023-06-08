-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.PublishDiagnosticsParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.Diagnostic
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
The publish diagnostic notification's parameters.
-}
data PublishDiagnosticsParams = PublishDiagnosticsParams 
  { {-|
  The URI for which diagnostic information is reported.
  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  Optional the version number of the document the diagnostics are published for.

  @since 3.15.0
  -}
  _version :: (Maybe Language.LSP.Protocol.Types.Common.Int32)
  , {-|
  An array of diagnostic information items.
  -}
  _diagnostics :: [Language.LSP.Protocol.Internal.Types.Diagnostic.Diagnostic]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON PublishDiagnosticsParams where
  toJSON (PublishDiagnosticsParams arg0 arg1 arg2) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,"version" Language.LSP.Protocol.Types.Common..=? arg1
    ,["diagnostics" Aeson..= arg2]]

instance Aeson.FromJSON PublishDiagnosticsParams where
  parseJSON = Aeson.withObject "PublishDiagnosticsParams" $ \arg -> PublishDiagnosticsParams <$> arg Aeson..: "uri" <*> arg Aeson..:! "version" <*> arg Aeson..: "diagnostics"
