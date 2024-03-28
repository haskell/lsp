{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.PartialResultParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data PartialResultParams = PartialResultParams 
  { {-|
  An optional token that a server can use to report partial results (e.g. streaming) to
  the client.
  -}
  partialResultToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON PartialResultParams)

instance Aeson.ToJSON PartialResultParams where
  toJSON (PartialResultParams arg0) = Aeson.object $ concat $  ["partialResultToken" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON PartialResultParams where
  parseJSON = Aeson.withObject "PartialResultParams" $ \arg -> PartialResultParams <$> arg Language.LSP.Protocol.Types.Common..:!? "partialResultToken"
