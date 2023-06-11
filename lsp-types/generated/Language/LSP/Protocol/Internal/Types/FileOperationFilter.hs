-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FileOperationFilter where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.FileOperationPattern
import qualified Language.LSP.Protocol.Types.Common

{-|
A filter to describe in which file operation requests or notifications
the server is interested in receiving.

@since 3.16.0
-}
data FileOperationFilter = FileOperationFilter 
  { {-|
  A Uri scheme like `file` or `untitled`.
  -}
  _scheme :: (Maybe Data.Text.Text)
  , {-|
  The actual file operation pattern.
  -}
  _pattern :: Language.LSP.Protocol.Internal.Types.FileOperationPattern.FileOperationPattern
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON FileOperationFilter where
  toJSON (FileOperationFilter arg0 arg1) = Aeson.object $ concat $  ["scheme" Language.LSP.Protocol.Types.Common..=? arg0
    ,["pattern" Aeson..= arg1]]

instance Aeson.FromJSON FileOperationFilter where
  parseJSON = Aeson.withObject "FileOperationFilter" $ \arg -> FileOperationFilter <$> arg Aeson..:! "scheme" <*> arg Aeson..: "pattern"
