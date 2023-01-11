-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ExecutionSummary where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data ExecutionSummary = ExecutionSummary 
  { {-|
  A strict monotonically increasing value
  indicating the execution order of a cell
  inside a notebook.

  -}
  _executionOrder :: Language.LSP.Protocol.Types.Common.UInt
  , {-|
  Whether the execution was successful or
  not if known by the client.

  -}
  _success :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON ExecutionSummary where
  toJSON (ExecutionSummary arg0 arg1) = Aeson.object $ concat $  [["executionOrder" Aeson..= arg0]
    ,"success" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON ExecutionSummary where
  parseJSON = Aeson.withObject "ExecutionSummary" $ \arg -> ExecutionSummary <$> arg Aeson..: "executionOrder" <*> arg Aeson..:! "success"