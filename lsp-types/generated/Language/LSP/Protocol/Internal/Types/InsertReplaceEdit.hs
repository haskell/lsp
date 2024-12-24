{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InsertReplaceEdit where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
A special text edit to provide an insert and a replace operation.

@since 3.16.0
-}
data InsertReplaceEdit = InsertReplaceEdit 
  { {-|
  The string to be inserted.
  -}
  newText :: Data.Text.Text
  , {-|
  The range if the insert is requested
  -}
  insert :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The range if the replace is requested.
  -}
  replace :: Language.LSP.Protocol.Internal.Types.Range.Range
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InsertReplaceEdit)

instance Aeson.ToJSON InsertReplaceEdit where
  toJSON (InsertReplaceEdit arg0 arg1 arg2) = Aeson.object $ concat $  [["newText" Aeson..= arg0]
    ,["insert" Aeson..= arg1]
    ,["replace" Aeson..= arg2]]

instance Aeson.FromJSON InsertReplaceEdit where
  parseJSON = Aeson.withObject "InsertReplaceEdit" $ \arg -> InsertReplaceEdit <$> arg Aeson..: "newText" <*> arg Aeson..: "insert" <*> arg Aeson..: "replace"
