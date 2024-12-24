{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesClientCapabilities where

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

-}
data DidChangeWatchedFilesClientCapabilities = DidChangeWatchedFilesClientCapabilities 
  { {-|
  Did change watched files notification supports dynamic registration. Please note
  that the current protocol doesn't support static configuration for file changes
  from the server side.
  -}
  dynamicRegistration :: (Maybe Bool)
  , {-|
  Whether the client has support for `RelativePattern`
  or not.

  @since 3.17.0
  -}
  relativePatternSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DidChangeWatchedFilesClientCapabilities)

instance Aeson.ToJSON DidChangeWatchedFilesClientCapabilities where
  toJSON (DidChangeWatchedFilesClientCapabilities arg0 arg1) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"relativePatternSupport" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON DidChangeWatchedFilesClientCapabilities where
  parseJSON = Aeson.withObject "DidChangeWatchedFilesClientCapabilities" $ \arg -> DidChangeWatchedFilesClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "relativePatternSupport"
