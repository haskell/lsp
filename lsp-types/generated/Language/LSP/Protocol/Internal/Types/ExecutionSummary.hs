{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ExecutionSummary where

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

-}
data ExecutionSummary = ExecutionSummary 
  { {-|
  A strict monotonically increasing value
  indicating the execution order of a cell
  inside a notebook.
  -}
  executionOrder :: Language.LSP.Protocol.Types.Common.UInt
  , {-|
  Whether the execution was successful or
  not if known by the client.
  -}
  success :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ExecutionSummary)

instance Aeson.ToJSON ExecutionSummary where
  toJSON (ExecutionSummary arg0 arg1) = Aeson.object $ concat $  [["executionOrder" Aeson..= arg0]
    ,"success" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON ExecutionSummary where
  parseJSON = Aeson.withObject "ExecutionSummary" $ \arg -> ExecutionSummary <$> arg Aeson..: "executionOrder" <*> arg Language.LSP.Protocol.Types.Common..:!? "success"
