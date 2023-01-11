-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ApplyWorkspaceEditParams where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceEdit
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters passed via a apply workspace edit request.

-}
data ApplyWorkspaceEditParams = ApplyWorkspaceEditParams 
  { {-|
  An optional label of the workspace edit. This label is
  presented in the user interface for example on an undo
  stack to undo the workspace edit.

  -}
  _label :: (Maybe Data.Text.Text)
  , {-|
  The edits to apply.

  -}
  _edit :: Language.LSP.Protocol.Internal.Types.WorkspaceEdit.WorkspaceEdit
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON ApplyWorkspaceEditParams where
  toJSON (ApplyWorkspaceEditParams arg0 arg1) = Aeson.object $ concat $  ["label" Language.LSP.Protocol.Types.Common..=? arg0
    ,["edit" Aeson..= arg1]]

instance Aeson.FromJSON ApplyWorkspaceEditParams where
  parseJSON = Aeson.withObject "ApplyWorkspaceEditParams" $ \arg -> ApplyWorkspaceEditParams <$> arg Aeson..:! "label" <*> arg Aeson..: "edit"