-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.PartialResultParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data PartialResultParams = PartialResultParams 
  { {-|
  An optional token that a server can use to report partial results (e.g. streaming) to
  the client.
  -}
  _partialResultToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON PartialResultParams where
  toJSON (PartialResultParams arg0) = Aeson.object $ concat $  ["partialResultToken" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON PartialResultParams where
  parseJSON = Aeson.withObject "PartialResultParams" $ \arg -> PartialResultParams <$> arg Aeson..:! "partialResultToken"
