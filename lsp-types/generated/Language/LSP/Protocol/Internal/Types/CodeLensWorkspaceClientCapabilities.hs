-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeLensWorkspaceClientCapabilities where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.16.0

-}
data CodeLensWorkspaceClientCapabilities = CodeLensWorkspaceClientCapabilities 
  { {-|
  Whether the client implementation supports a refresh request sent from the
  server to the client.

  Note that this event is global and will force the client to refresh all
  code lenses currently shown. It should be used with absolute care and is
  useful for situation where a server for example detect a project wide
  change that requires such a calculation.

  -}
  _refreshSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON CodeLensWorkspaceClientCapabilities where
  toJSON (CodeLensWorkspaceClientCapabilities arg0) = Aeson.object $ concat $  ["refreshSupport" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON CodeLensWorkspaceClientCapabilities where
  parseJSON = Aeson.withObject "CodeLensWorkspaceClientCapabilities" $ \arg -> CodeLensWorkspaceClientCapabilities <$> arg Aeson..:! "refreshSupport"