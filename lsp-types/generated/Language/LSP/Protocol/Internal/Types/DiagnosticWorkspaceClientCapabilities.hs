{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DiagnosticWorkspaceClientCapabilities where

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
Workspace client capabilities specific to diagnostic pull requests.

@since 3.17.0
-}
data DiagnosticWorkspaceClientCapabilities = DiagnosticWorkspaceClientCapabilities 
  { {-|
  Whether the client implementation supports a refresh request sent from
  the server to the client.

  Note that this event is global and will force the client to refresh all
  pulled diagnostics currently shown. It should be used with absolute care and
  is useful for situation where a server for example detects a project wide
  change that requires such a calculation.
  -}
  _refreshSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DiagnosticWorkspaceClientCapabilities)

instance Aeson.ToJSON DiagnosticWorkspaceClientCapabilities where
  toJSON (DiagnosticWorkspaceClientCapabilities arg0) = Aeson.object $ concat $  ["refreshSupport" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON DiagnosticWorkspaceClientCapabilities where
  parseJSON = Aeson.withObject "DiagnosticWorkspaceClientCapabilities" $ \arg -> DiagnosticWorkspaceClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "refreshSupport"
