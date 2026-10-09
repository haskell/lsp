{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingClientCapabilities where

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
Client capabilities of a `DocumentRangeFormattingRequest`.
-}
data DocumentRangeFormattingClientCapabilities = DocumentRangeFormattingClientCapabilities 
  { {-|
  Whether range formatting supports dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  Whether the client supports formatting multiple ranges at once.

  @since 3.18.0
  -}
  _rangesSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentRangeFormattingClientCapabilities)

instance Aeson.ToJSON DocumentRangeFormattingClientCapabilities where
  toJSON (DocumentRangeFormattingClientCapabilities arg0 arg1) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"rangesSupport" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON DocumentRangeFormattingClientCapabilities where
  parseJSON = Aeson.withObject "DocumentRangeFormattingClientCapabilities" $ \arg -> DocumentRangeFormattingClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "rangesSupport"
