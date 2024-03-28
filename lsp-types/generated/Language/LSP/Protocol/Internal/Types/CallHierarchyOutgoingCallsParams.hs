{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CallHierarchyOutgoingCallsParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyItem
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameter of a `callHierarchy/outgoingCalls` request.

@since 3.16.0
-}
data CallHierarchyOutgoingCallsParams = CallHierarchyOutgoingCallsParams 
  { {-|
  An optional token that a server can use to report work done progress.
  -}
  workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  An optional token that a server can use to report partial results (e.g. streaming) to
  the client.
  -}
  partialResultToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|

  -}
  item :: Language.LSP.Protocol.Internal.Types.CallHierarchyItem.CallHierarchyItem
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CallHierarchyOutgoingCallsParams)

instance Aeson.ToJSON CallHierarchyOutgoingCallsParams where
  toJSON (CallHierarchyOutgoingCallsParams arg0 arg1 arg2) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,"partialResultToken" Language.LSP.Protocol.Types.Common..=? arg1
    ,["item" Aeson..= arg2]]

instance Aeson.FromJSON CallHierarchyOutgoingCallsParams where
  parseJSON = Aeson.withObject "CallHierarchyOutgoingCallsParams" $ \arg -> CallHierarchyOutgoingCallsParams <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneToken" <*> arg Language.LSP.Protocol.Types.Common..:!? "partialResultToken" <*> arg Aeson..: "item"
