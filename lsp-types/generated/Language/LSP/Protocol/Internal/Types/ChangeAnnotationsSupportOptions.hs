{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ChangeAnnotationsSupportOptions where

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
@since 3.18.0
@proposed
-}
data ChangeAnnotationsSupportOptions = ChangeAnnotationsSupportOptions 
  { {-|
  Whether the client groups edits with equal labels into tree nodes,
  for instance all edits labelled with "Changes in Strings" would
  be a tree node.
  -}
  groupsOnLabel :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ChangeAnnotationsSupportOptions)

instance Aeson.ToJSON ChangeAnnotationsSupportOptions where
  toJSON (ChangeAnnotationsSupportOptions arg0) = Aeson.object $ concat $  ["groupsOnLabel" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON ChangeAnnotationsSupportOptions where
  parseJSON = Aeson.withObject "ChangeAnnotationsSupportOptions" $ \arg -> ChangeAnnotationsSupportOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "groupsOnLabel"
