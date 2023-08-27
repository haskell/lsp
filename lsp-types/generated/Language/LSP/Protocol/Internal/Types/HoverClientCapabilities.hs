{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.HoverClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.MarkupKind
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data HoverClientCapabilities = HoverClientCapabilities 
  { {-|
  Whether hover supports dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  Client supports the following content formats for the content
  property. The order describes the preferred format of the client.
  -}
  _contentFormat :: (Maybe [Language.LSP.Protocol.Internal.Types.MarkupKind.MarkupKind])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON HoverClientCapabilities)

instance Aeson.ToJSON HoverClientCapabilities where
  toJSON (HoverClientCapabilities arg0 arg1) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"contentFormat" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON HoverClientCapabilities where
  parseJSON = Aeson.withObject "HoverClientCapabilities" $ \arg -> HoverClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "contentFormat"
