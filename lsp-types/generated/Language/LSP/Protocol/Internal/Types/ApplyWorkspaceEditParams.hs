{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ApplyWorkspaceEditParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceEdit
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters passed via an apply workspace edit request.
-}
data ApplyWorkspaceEditParams = ApplyWorkspaceEditParams 
  { {-|
  An optional label of the workspace edit. This label is
  presented in the user interface for example on an undo
  stack to undo the workspace edit.
  -}
  label :: (Maybe Data.Text.Text)
  , {-|
  The edits to apply.
  -}
  edit :: Language.LSP.Protocol.Internal.Types.WorkspaceEdit.WorkspaceEdit
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ApplyWorkspaceEditParams)

instance Aeson.ToJSON ApplyWorkspaceEditParams where
  toJSON (ApplyWorkspaceEditParams arg0 arg1) = Aeson.object $ concat $  ["label" Language.LSP.Protocol.Types.Common..=? arg0
    ,["edit" Aeson..= arg1]]

instance Aeson.FromJSON ApplyWorkspaceEditParams where
  parseJSON = Aeson.withObject "ApplyWorkspaceEditParams" $ \arg -> ApplyWorkspaceEditParams <$> arg Language.LSP.Protocol.Types.Common..:!? "label" <*> arg Aeson..: "edit"
