-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CancelParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data CancelParams = CancelParams 
  { {-|
  The request id to cancel.
  -}
  _id :: (Language.LSP.Protocol.Types.Common.Int32 Language.LSP.Protocol.Types.Common.|? Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON CancelParams where
  toJSON (CancelParams arg0) = Aeson.object $ concat $  [["id" Aeson..= arg0]]

instance Aeson.FromJSON CancelParams where
  parseJSON = Aeson.withObject "CancelParams" $ \arg -> CancelParams <$> arg Aeson..: "id"
