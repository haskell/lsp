-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FileOperationRegistrationOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.FileOperationFilter
import qualified Language.LSP.Protocol.Types.Common

{-|
The options to register for file operations.

@since 3.16.0

-}
data FileOperationRegistrationOptions = FileOperationRegistrationOptions 
  { {-|
  The actual filters.

  -}
  _filters :: [Language.LSP.Protocol.Internal.Types.FileOperationFilter.FileOperationFilter]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON FileOperationRegistrationOptions where
  toJSON (FileOperationRegistrationOptions arg0) = Aeson.object $ concat $  [["filters" Aeson..= arg0]]

instance Aeson.FromJSON FileOperationRegistrationOptions where
  parseJSON = Aeson.withObject "FileOperationRegistrationOptions" $ \arg -> FileOperationRegistrationOptions <$> arg Aeson..: "filters"