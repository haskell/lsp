-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CallHierarchyOutgoingCall where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyItem
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents an outgoing call, e.g. calling a getter from a method or a method from a constructor etc.

@since 3.16.0
-}
data CallHierarchyOutgoingCall = CallHierarchyOutgoingCall 
  { {-|
  The item that is called.
  -}
  _to :: Language.LSP.Protocol.Internal.Types.CallHierarchyItem.CallHierarchyItem
  , {-|
  The range at which this item is called. This is the range relative to the caller, e.g the item
  passed to `CallHierarchyItemProvider.provideCallHierarchyOutgoingCalls`
  and not `CallHierarchyOutgoingCall.to`.
  -}
  _fromRanges :: [Language.LSP.Protocol.Internal.Types.Range.Range]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON CallHierarchyOutgoingCall where
  toJSON (CallHierarchyOutgoingCall arg0 arg1) = Aeson.object $ concat $  [["to" Aeson..= arg0]
    ,["fromRanges" Aeson..= arg1]]

instance Aeson.FromJSON CallHierarchyOutgoingCall where
  parseJSON = Aeson.withObject "CallHierarchyOutgoingCall" $ \arg -> CallHierarchyOutgoingCall <$> arg Aeson..: "to" <*> arg Aeson..: "fromRanges"
