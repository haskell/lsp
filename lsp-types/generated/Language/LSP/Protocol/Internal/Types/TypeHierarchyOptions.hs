-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TypeHierarchyOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Type hierarchy options used during static registration.

@since 3.17.0
-}
data TypeHierarchyOptions = TypeHierarchyOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON TypeHierarchyOptions where
  toJSON (TypeHierarchyOptions arg0) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON TypeHierarchyOptions where
  parseJSON = Aeson.withObject "TypeHierarchyOptions" $ \arg -> TypeHierarchyOptions <$> arg Aeson..:! "workDoneProgress"