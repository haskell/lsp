{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.MessageActionItem where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data MessageActionItem = MessageActionItem 
  { {-|
  A short title like 'Retry', 'Open Log' etc.
  -}
  _title :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON MessageActionItem)

instance Aeson.ToJSON MessageActionItem where
  toJSON (MessageActionItem arg0) = Aeson.object $ concat $  [["title" Aeson..= arg0]]

instance Aeson.FromJSON MessageActionItem where
  parseJSON = Aeson.withObject "MessageActionItem" $ \arg -> MessageActionItem <$> arg Aeson..: "title"
