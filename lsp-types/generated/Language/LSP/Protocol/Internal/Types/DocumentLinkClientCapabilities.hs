-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentLinkClientCapabilities where

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
The client capabilities of a `DocumentLinkRequest`.
-}
data DocumentLinkClientCapabilities = DocumentLinkClientCapabilities 
  { {-|
  Whether document link supports dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  Whether the client supports the `tooltip` property on `DocumentLink`.

  @since 3.15.0
  -}
  _tooltipSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentLinkClientCapabilities)

instance Aeson.ToJSON DocumentLinkClientCapabilities where
  toJSON (DocumentLinkClientCapabilities arg0 arg1) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"tooltipSupport" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON DocumentLinkClientCapabilities where
  parseJSON = Aeson.withObject "DocumentLinkClientCapabilities" $ \arg -> DocumentLinkClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "tooltipSupport"
