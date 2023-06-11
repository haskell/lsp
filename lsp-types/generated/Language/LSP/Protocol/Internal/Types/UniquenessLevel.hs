-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.UniquenessLevel where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Set
import qualified Data.String
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
Moniker uniqueness level to define scope of the moniker.

@since 3.16.0
-}
data UniquenessLevel = 
    {-|
  The moniker is only unique inside a document
  -}
  UniquenessLevel_Document
  | {-|
  The moniker is unique inside a project for which a dump got created
  -}
  UniquenessLevel_Project
  | {-|
  The moniker is unique inside the group to which a project belongs
  -}
  UniquenessLevel_Group
  | {-|
  The moniker is unique inside the moniker scheme.
  -}
  UniquenessLevel_Scheme
  | {-|
  The moniker is globally unique
  -}
  UniquenessLevel_Global
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum UniquenessLevel Data.Text.Text)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum UniquenessLevel where
  knownValues = Data.Set.fromList [UniquenessLevel_Document
    ,UniquenessLevel_Project
    ,UniquenessLevel_Group
    ,UniquenessLevel_Scheme
    ,UniquenessLevel_Global]
  type EnumBaseType UniquenessLevel = Data.Text.Text
  toEnumBaseType UniquenessLevel_Document = "document"
  toEnumBaseType UniquenessLevel_Project = "project"
  toEnumBaseType UniquenessLevel_Group = "group"
  toEnumBaseType UniquenessLevel_Scheme = "scheme"
  toEnumBaseType UniquenessLevel_Global = "global"
  fromEnumBaseType "document" = pure UniquenessLevel_Document
  fromEnumBaseType "project" = pure UniquenessLevel_Project
  fromEnumBaseType "group" = pure UniquenessLevel_Group
  fromEnumBaseType "scheme" = pure UniquenessLevel_Scheme
  fromEnumBaseType "global" = pure UniquenessLevel_Global
  fromEnumBaseType _ = Nothing


