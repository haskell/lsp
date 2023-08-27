{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.FileEvent
import qualified Language.LSP.Protocol.Types.Common

{-|
The watched files change notification's parameters.
-}
data DidChangeWatchedFilesParams = DidChangeWatchedFilesParams 
  { {-|
  The actual file events.
  -}
  _changes :: [Language.LSP.Protocol.Internal.Types.FileEvent.FileEvent]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DidChangeWatchedFilesParams)

instance Aeson.ToJSON DidChangeWatchedFilesParams where
  toJSON (DidChangeWatchedFilesParams arg0) = Aeson.object $ concat $  [["changes" Aeson..= arg0]]

instance Aeson.FromJSON DidChangeWatchedFilesParams where
  parseJSON = Aeson.withObject "DidChangeWatchedFilesParams" $ \arg -> DidChangeWatchedFilesParams <$> arg Aeson..: "changes"
