{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ResourceOperation where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
A generic resource operation.
-}
data ResourceOperation = ResourceOperation 
  { {-|
  The resource operation kind.
  -}
  kind :: Data.Text.Text
  , {-|
  An optional annotation identifier describing the operation.

  @since 3.16.0
  -}
  annotationId :: (Maybe Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier.ChangeAnnotationIdentifier)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ResourceOperation)

instance Aeson.ToJSON ResourceOperation where
  toJSON (ResourceOperation arg0 arg1) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,"annotationId" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON ResourceOperation where
  parseJSON = Aeson.withObject "ResourceOperation" $ \arg -> ResourceOperation <$> arg Aeson..: "kind" <*> arg Language.LSP.Protocol.Types.Common..:!? "annotationId"
