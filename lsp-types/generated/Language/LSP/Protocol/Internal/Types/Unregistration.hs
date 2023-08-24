{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Unregistration where

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
General parameters to unregister a request or notification.
-}
data Unregistration = Unregistration 
  { {-|
  The id used to unregister the request or notification. Usually an id
  provided during the register request.
  -}
  _id :: Data.Text.Text
  , {-|
  The method to unregister for.
  -}
  _method :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON Unregistration)

instance Aeson.ToJSON Unregistration where
  toJSON (Unregistration arg0 arg1) = Aeson.object $ concat $  [["id" Aeson..= arg0]
    ,["method" Aeson..= arg1]]

instance Aeson.FromJSON Unregistration where
  parseJSON = Aeson.withObject "Unregistration" $ \arg -> Unregistration <$> arg Aeson..: "id" <*> arg Aeson..: "method"
