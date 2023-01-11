-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FileOperationPatternOptions where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Matching options for the file operation pattern.

@since 3.16.0

-}
data FileOperationPatternOptions = FileOperationPatternOptions 
  { {-|
  The pattern should be matched ignoring casing.

  -}
  _ignoreCase :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON FileOperationPatternOptions where
  toJSON (FileOperationPatternOptions arg0) = Aeson.object $ concat $  ["ignoreCase" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON FileOperationPatternOptions where
  parseJSON = Aeson.withObject "FileOperationPatternOptions" $ \arg -> FileOperationPatternOptions <$> arg Aeson..:! "ignoreCase"