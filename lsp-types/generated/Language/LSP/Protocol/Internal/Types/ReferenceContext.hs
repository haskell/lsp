-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ReferenceContext where

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
Value-object that contains additional information when
requesting references.
-}
data ReferenceContext = ReferenceContext 
  { {-|
  Include the declaration of the current symbol.
  -}
  _includeDeclaration :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ReferenceContext)

instance Aeson.ToJSON ReferenceContext where
  toJSON (ReferenceContext arg0) = Aeson.object $ concat $  [["includeDeclaration" Aeson..= arg0]]

instance Aeson.FromJSON ReferenceContext where
  parseJSON = Aeson.withObject "ReferenceContext" $ \arg -> ReferenceContext <$> arg Aeson..: "includeDeclaration"
