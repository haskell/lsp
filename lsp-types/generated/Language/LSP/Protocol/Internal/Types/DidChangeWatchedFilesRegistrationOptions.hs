{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidChangeWatchedFilesRegistrationOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.FileSystemWatcher
import qualified Language.LSP.Protocol.Types.Common

{-|
Describe options to be used when registered for text document change events.
-}
data DidChangeWatchedFilesRegistrationOptions = DidChangeWatchedFilesRegistrationOptions 
  { {-|
  The watchers to register.
  -}
  _watchers :: [Language.LSP.Protocol.Internal.Types.FileSystemWatcher.FileSystemWatcher]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DidChangeWatchedFilesRegistrationOptions)

instance Aeson.ToJSON DidChangeWatchedFilesRegistrationOptions where
  toJSON (DidChangeWatchedFilesRegistrationOptions arg0) = Aeson.object $ concat $  [["watchers" Aeson..= arg0]]

instance Aeson.FromJSON DidChangeWatchedFilesRegistrationOptions where
  parseJSON = Aeson.withObject "DidChangeWatchedFilesRegistrationOptions" $ \arg -> DidChangeWatchedFilesRegistrationOptions <$> arg Aeson..: "watchers"
