-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CallHierarchyIncomingCall where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.CallHierarchyItem
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents an incoming call, e.g. a caller of a method or constructor.

@since 3.16.0

-}
data CallHierarchyIncomingCall = CallHierarchyIncomingCall 
  { {-|
  The item that makes the call.

  -}
  _from :: Language.LSP.Protocol.Internal.Types.CallHierarchyItem.CallHierarchyItem
  , {-|
  The ranges at which the calls appear. This is relative to the caller
  denoted by `CallHierarchyIncomingCall.from`.

  -}
  _fromRanges :: [Language.LSP.Protocol.Internal.Types.Range.Range]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON CallHierarchyIncomingCall where
  toJSON (CallHierarchyIncomingCall arg0 arg1) = Aeson.object $ concat $  [["from" Aeson..= arg0]
    ,["fromRanges" Aeson..= arg1]]

instance Aeson.FromJSON CallHierarchyIncomingCall where
  parseJSON = Aeson.withObject "CallHierarchyIncomingCall" $ \arg -> CallHierarchyIncomingCall <$> arg Aeson..: "from" <*> arg Aeson..: "fromRanges"