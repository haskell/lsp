{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.StaleRequestSupportOptions where

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
@since 3.18.0
@proposed
-}
data StaleRequestSupportOptions = StaleRequestSupportOptions 
  { {-|
  The client will actively cancel the request.
  -}
  _cancel :: Bool
  , {-|
  The list of requests for which the client
  will retry the request if it receives a
  response with error code `ContentModified`
  -}
  _retryOnContentModified :: [Data.Text.Text]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON StaleRequestSupportOptions)

instance Aeson.ToJSON StaleRequestSupportOptions where
  toJSON (StaleRequestSupportOptions arg0 arg1) = Aeson.object $ concat $  [["cancel" Aeson..= arg0]
    ,["retryOnContentModified" Aeson..= arg1]]

instance Aeson.FromJSON StaleRequestSupportOptions where
  parseJSON = Aeson.withObject "StaleRequestSupportOptions" $ \arg -> StaleRequestSupportOptions <$> arg Aeson..: "cancel" <*> arg Aeson..: "retryOnContentModified"
