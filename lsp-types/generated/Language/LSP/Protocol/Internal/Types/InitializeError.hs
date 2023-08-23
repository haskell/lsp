-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InitializeError where

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
The data type of the ResponseError if the
initialize request fails.
-}
data InitializeError = InitializeError 
  { {-|
  Indicates whether the client execute the following retry logic:
  (1) show the message provided by the ResponseError to the user
  (2) user selects retry or cancel
  (3) if user selected retry the initialize method is sent again.
  -}
  _retry :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InitializeError)

instance Aeson.ToJSON InitializeError where
  toJSON (InitializeError arg0) = Aeson.object $ concat $  [["retry" Aeson..= arg0]]

instance Aeson.FromJSON InitializeError where
  parseJSON = Aeson.withObject "InitializeError" $ \arg -> InitializeError <$> arg Aeson..: "retry"
