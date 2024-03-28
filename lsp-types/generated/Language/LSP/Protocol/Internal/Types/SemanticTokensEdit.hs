{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensEdit where

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
@since 3.16.0
-}
data SemanticTokensEdit = SemanticTokensEdit 
  { {-|
  The start offset of the edit.
  -}
  start :: Language.LSP.Protocol.Types.Common.UInt
  , {-|
  The count of elements to remove.
  -}
  deleteCount :: Language.LSP.Protocol.Types.Common.UInt
  , {-|
  The elements to insert.
  -}
  data_ :: (Maybe [Language.LSP.Protocol.Types.Common.UInt])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SemanticTokensEdit)

instance Aeson.ToJSON SemanticTokensEdit where
  toJSON (SemanticTokensEdit arg0 arg1 arg2) = Aeson.object $ concat $  [["start" Aeson..= arg0]
    ,["deleteCount" Aeson..= arg1]
    ,"data" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON SemanticTokensEdit where
  parseJSON = Aeson.withObject "SemanticTokensEdit" $ \arg -> SemanticTokensEdit <$> arg Aeson..: "start" <*> arg Aeson..: "deleteCount" <*> arg Language.LSP.Protocol.Types.Common..:!? "data"
