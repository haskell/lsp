{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineValueWorkspaceClientCapabilities where

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
Client workspace capabilities specific to inline values.

@since 3.17.0
-}
data InlineValueWorkspaceClientCapabilities = InlineValueWorkspaceClientCapabilities 
  { {-|
  Whether the client implementation supports a refresh request sent from the
  server to the client.

  Note that this event is global and will force the client to refresh all
  inline values currently shown. It should be used with absolute care and is
  useful for situation where a server for example detects a project wide
  change that requires such a calculation.
  -}
  refreshSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InlineValueWorkspaceClientCapabilities)

instance Aeson.ToJSON InlineValueWorkspaceClientCapabilities where
  toJSON (InlineValueWorkspaceClientCapabilities arg0) = Aeson.object $ concat $  ["refreshSupport" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON InlineValueWorkspaceClientCapabilities where
  parseJSON = Aeson.withObject "InlineValueWorkspaceClientCapabilities" $ \arg -> InlineValueWorkspaceClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "refreshSupport"
