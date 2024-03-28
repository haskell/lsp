{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DidChangeWorkspaceFoldersParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFoldersChangeEvent
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters of a `workspace/didChangeWorkspaceFolders` notification.
-}
data DidChangeWorkspaceFoldersParams = DidChangeWorkspaceFoldersParams 
  { {-|
  The actual workspace folder change event.
  -}
  event :: Language.LSP.Protocol.Internal.Types.WorkspaceFoldersChangeEvent.WorkspaceFoldersChangeEvent
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DidChangeWorkspaceFoldersParams)

instance Aeson.ToJSON DidChangeWorkspaceFoldersParams where
  toJSON (DidChangeWorkspaceFoldersParams arg0) = Aeson.object $ concat $  [["event" Aeson..= arg0]]

instance Aeson.FromJSON DidChangeWorkspaceFoldersParams where
  parseJSON = Aeson.withObject "DidChangeWorkspaceFoldersParams" $ \arg -> DidChangeWorkspaceFoldersParams <$> arg Aeson..: "event"
