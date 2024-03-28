{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FileOperationPatternOptions where

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
Matching options for the file operation pattern.

@since 3.16.0
-}
data FileOperationPatternOptions = FileOperationPatternOptions 
  { {-|
  The pattern should be matched ignoring casing.
  -}
  ignoreCase :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON FileOperationPatternOptions)

instance Aeson.ToJSON FileOperationPatternOptions where
  toJSON (FileOperationPatternOptions arg0) = Aeson.object $ concat $  ["ignoreCase" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON FileOperationPatternOptions where
  parseJSON = Aeson.withObject "FileOperationPatternOptions" $ \arg -> FileOperationPatternOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "ignoreCase"
