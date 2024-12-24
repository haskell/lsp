{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CancelParams where

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
data CancelParams = CancelParams 
  { {-|
  The request id to cancel.
  -}
  id :: (Language.LSP.Protocol.Types.Common.Int32 Language.LSP.Protocol.Types.Common.|? Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CancelParams)

instance Aeson.ToJSON CancelParams where
  toJSON (CancelParams arg0) = Aeson.object $ concat $  [["id" Aeson..= arg0]]

instance Aeson.FromJSON CancelParams where
  parseJSON = Aeson.withObject "CancelParams" $ \arg -> CancelParams <$> arg Aeson..: "id"
