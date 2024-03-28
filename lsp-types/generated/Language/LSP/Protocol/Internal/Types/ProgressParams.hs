{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ProgressParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data ProgressParams = ProgressParams 
  { {-|
  The progress token provided by the client or server.
  -}
  token :: Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken
  , {-|
  The progress data.
  -}
  value :: Data.Aeson.Value
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ProgressParams)

instance Aeson.ToJSON ProgressParams where
  toJSON (ProgressParams arg0 arg1) = Aeson.object $ concat $  [["token" Aeson..= arg0]
    ,["value" Aeson..= arg1]]

instance Aeson.FromJSON ProgressParams where
  parseJSON = Aeson.withObject "ProgressParams" $ \arg -> ProgressParams <$> arg Aeson..: "token" <*> arg Aeson..: "value"
