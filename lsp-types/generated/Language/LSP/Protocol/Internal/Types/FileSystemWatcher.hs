{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FileSystemWatcher where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.GlobPattern
import qualified Language.LSP.Protocol.Internal.Types.WatchKind
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data FileSystemWatcher = FileSystemWatcher 
  { {-|
  The glob pattern to watch. See `GlobPattern` for more detail.

  @since 3.17.0 support for relative patterns.
  -}
  _globPattern :: Language.LSP.Protocol.Internal.Types.GlobPattern.GlobPattern
  , {-|
  The kind of events of interest. If omitted it defaults
  to WatchKind.Create | WatchKind.Change | WatchKind.Delete
  which is 7.
  -}
  _kind :: (Maybe Language.LSP.Protocol.Internal.Types.WatchKind.WatchKind)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON FileSystemWatcher)

instance Aeson.ToJSON FileSystemWatcher where
  toJSON (FileSystemWatcher arg0 arg1) = Aeson.object $ concat $  [["globPattern" Aeson..= arg0]
    ,"kind" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON FileSystemWatcher where
  parseJSON = Aeson.withObject "FileSystemWatcher" $ \arg -> FileSystemWatcher <$> arg Aeson..: "globPattern" <*> arg Language.LSP.Protocol.Types.Common..:!? "kind"
