{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceFoldersInitializeParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFolder
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data WorkspaceFoldersInitializeParams = WorkspaceFoldersInitializeParams 
  { {-|
  The workspace folders configured in the client when the server starts.

  This property is only available if the client supports workspace folders.
  It can be `null` if the client supports workspace folders but none are
  configured.

  @since 3.6.0
  -}
  _workspaceFolders :: (Maybe ([Language.LSP.Protocol.Internal.Types.WorkspaceFolder.WorkspaceFolder] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkspaceFoldersInitializeParams)

instance Aeson.ToJSON WorkspaceFoldersInitializeParams where
  toJSON (WorkspaceFoldersInitializeParams arg0) = Aeson.object $ concat $  ["workspaceFolders" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON WorkspaceFoldersInitializeParams where
  parseJSON = Aeson.withObject "WorkspaceFoldersInitializeParams" $ \arg -> WorkspaceFoldersInitializeParams <$> arg Language.LSP.Protocol.Types.Common..:!? "workspaceFolders"
