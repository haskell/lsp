{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FileEvent where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.FileChangeType
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
An event describing a file change.
-}
data FileEvent = FileEvent 
  { {-|
  The file's uri.
  -}
  uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The change type.
  -}
  type_ :: Language.LSP.Protocol.Internal.Types.FileChangeType.FileChangeType
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON FileEvent)

instance Aeson.ToJSON FileEvent where
  toJSON (FileEvent arg0 arg1) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,["type" Aeson..= arg1]]

instance Aeson.FromJSON FileEvent where
  parseJSON = Aeson.withObject "FileEvent" $ \arg -> FileEvent <$> arg Aeson..: "uri" <*> arg Aeson..: "type"
