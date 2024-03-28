{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientCompletionItemResolveOptions where

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
data ClientCompletionItemResolveOptions = ClientCompletionItemResolveOptions 
  { {-|
  The properties that a client can resolve lazily.
  -}
  properties :: [Data.Text.Text]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientCompletionItemResolveOptions)

instance Aeson.ToJSON ClientCompletionItemResolveOptions where
  toJSON (ClientCompletionItemResolveOptions arg0) = Aeson.object $ concat $  [["properties" Aeson..= arg0]]

instance Aeson.FromJSON ClientCompletionItemResolveOptions where
  parseJSON = Aeson.withObject "ClientCompletionItemResolveOptions" $ \arg -> ClientCompletionItemResolveOptions <$> arg Aeson..: "properties"
