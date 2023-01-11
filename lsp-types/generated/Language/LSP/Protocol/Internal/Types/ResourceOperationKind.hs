-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ResourceOperationKind where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.LspEnum

{-|

-}
data ResourceOperationKind = 
    {-|
  Supports creating new files and folders.

  -}
  ResourceOperationKind_Create
  | {-|
  Supports renaming existing files and folders.

  -}
  ResourceOperationKind_Rename
  | {-|
  Supports deleting existing files and folders.

  -}
  ResourceOperationKind_Delete
  deriving stock (Show, Eq, Ord, Generic)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum ResourceOperationKind Data.Text.Text)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum ResourceOperationKind where
  knownValues = Data.Set.fromList [ResourceOperationKind_Create
    ,ResourceOperationKind_Rename
    ,ResourceOperationKind_Delete]
  type EnumBaseType ResourceOperationKind = Data.Text.Text
  toEnumBaseType ResourceOperationKind_Create = "create"
  toEnumBaseType ResourceOperationKind_Rename = "rename"
  toEnumBaseType ResourceOperationKind_Delete = "delete"
  fromEnumBaseType "create" = pure ResourceOperationKind_Create
  fromEnumBaseType "rename" = pure ResourceOperationKind_Rename
  fromEnumBaseType "delete" = pure ResourceOperationKind_Delete
  fromEnumBaseType _ = Nothing

