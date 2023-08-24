-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceFolder where

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
import qualified Language.LSP.Protocol.Types.Uri

{-|
A workspace folder inside a client.
-}
data WorkspaceFolder = WorkspaceFolder 
  { {-|
  The associated URI for this workspace folder.
  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The name of the workspace folder. Used to refer to this
  workspace folder in the user interface.
  -}
  _name :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkspaceFolder)

instance Aeson.ToJSON WorkspaceFolder where
  toJSON (WorkspaceFolder arg0 arg1) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,["name" Aeson..= arg1]]

instance Aeson.FromJSON WorkspaceFolder where
  parseJSON = Aeson.withObject "WorkspaceFolder" $ \arg -> WorkspaceFolder <$> arg Aeson..: "uri" <*> arg Aeson..: "name"
