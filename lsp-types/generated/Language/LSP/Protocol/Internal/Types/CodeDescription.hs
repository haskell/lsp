-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeDescription where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
Structure to capture a description for an error code.

@since 3.16.0
-}
data CodeDescription = CodeDescription 
  { {-|
  An URI to open with more information about the diagnostic error.
  -}
  _href :: Language.LSP.Protocol.Types.Uri.Uri
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON CodeDescription where
  toJSON (CodeDescription arg0) = Aeson.object $ concat $  [["href" Aeson..= arg0]]

instance Aeson.FromJSON CodeDescription where
  parseJSON = Aeson.withObject "CodeDescription" $ \arg -> CodeDescription <$> arg Aeson..: "href"
