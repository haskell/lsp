{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TypeHierarchyOptions where

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
Type hierarchy options used during static registration.

@since 3.17.0
-}
data TypeHierarchyOptions = TypeHierarchyOptions 
  { {-|

  -}
  workDoneProgress :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TypeHierarchyOptions)

instance Aeson.ToJSON TypeHierarchyOptions where
  toJSON (TypeHierarchyOptions arg0) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON TypeHierarchyOptions where
  parseJSON = Aeson.withObject "TypeHierarchyOptions" $ \arg -> TypeHierarchyOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress"
