{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DiagnosticOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Diagnostic options.

@since 3.17.0
-}
data DiagnosticOptions = DiagnosticOptions 
  { {-|

  -}
  workDoneProgress :: (Maybe Bool)
  , {-|
  An optional identifier under which the diagnostics are
  managed by the client.
  -}
  identifier :: (Maybe Data.Text.Text)
  , {-|
  Whether the language has inter file dependencies meaning that
  editing code in one file can result in a different diagnostic
  set in another file. Inter file dependencies are common for
  most programming languages and typically uncommon for linters.
  -}
  interFileDependencies :: Bool
  , {-|
  The server provides support for workspace diagnostics as well.
  -}
  workspaceDiagnostics :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DiagnosticOptions)

instance Aeson.ToJSON DiagnosticOptions where
  toJSON (DiagnosticOptions arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"identifier" Language.LSP.Protocol.Types.Common..=? arg1
    ,["interFileDependencies" Aeson..= arg2]
    ,["workspaceDiagnostics" Aeson..= arg3]]

instance Aeson.FromJSON DiagnosticOptions where
  parseJSON = Aeson.withObject "DiagnosticOptions" $ \arg -> DiagnosticOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress" <*> arg Language.LSP.Protocol.Types.Common..:!? "identifier" <*> arg Aeson..: "interFileDependencies" <*> arg Aeson..: "workspaceDiagnostics"
