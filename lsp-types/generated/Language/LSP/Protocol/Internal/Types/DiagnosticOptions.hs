-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DiagnosticOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Diagnostic options.

@since 3.17.0
-}
data DiagnosticOptions = DiagnosticOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  An optional identifier under which the diagnostics are
  managed by the client.
  -}
  _identifier :: (Maybe Data.Text.Text)
  , {-|
  Whether the language has inter file dependencies meaning that
  editing code in one file can result in a different diagnostic
  set in another file. Inter file dependencies are common for
  most programming languages and typically uncommon for linters.
  -}
  _interFileDependencies :: Bool
  , {-|
  The server provides support for workspace diagnostics as well.
  -}
  _workspaceDiagnostics :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON DiagnosticOptions where
  toJSON (DiagnosticOptions arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"identifier" Language.LSP.Protocol.Types.Common..=? arg1
    ,["interFileDependencies" Aeson..= arg2]
    ,["workspaceDiagnostics" Aeson..= arg3]]

instance Aeson.FromJSON DiagnosticOptions where
  parseJSON = Aeson.withObject "DiagnosticOptions" $ \arg -> DiagnosticOptions <$> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "identifier" <*> arg Aeson..: "interFileDependencies" <*> arg Aeson..: "workspaceDiagnostics"