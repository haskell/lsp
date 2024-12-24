{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceSymbolOptions where

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
Server capabilities for a `WorkspaceSymbolRequest`.
-}
data WorkspaceSymbolOptions = WorkspaceSymbolOptions 
  { {-|

  -}
  workDoneProgress :: (Maybe Bool)
  , {-|
  The server provides support to resolve additional
  information for a workspace symbol.

  @since 3.17.0
  -}
  resolveProvider :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkspaceSymbolOptions)

instance Aeson.ToJSON WorkspaceSymbolOptions where
  toJSON (WorkspaceSymbolOptions arg0 arg1) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"resolveProvider" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON WorkspaceSymbolOptions where
  parseJSON = Aeson.withObject "WorkspaceSymbolOptions" $ \arg -> WorkspaceSymbolOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress" <*> arg Language.LSP.Protocol.Types.Common..:!? "resolveProvider"
