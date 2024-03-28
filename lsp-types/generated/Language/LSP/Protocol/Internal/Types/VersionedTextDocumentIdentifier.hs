{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.VersionedTextDocumentIdentifier where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A text document identifier to denote a specific version of a text document.
-}
data VersionedTextDocumentIdentifier = VersionedTextDocumentIdentifier 
  { {-|
  The text document's uri.
  -}
  uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The version number of this document.
  -}
  version :: Language.LSP.Protocol.Types.Common.Int32
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON VersionedTextDocumentIdentifier)

instance Aeson.ToJSON VersionedTextDocumentIdentifier where
  toJSON (VersionedTextDocumentIdentifier arg0 arg1) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,["version" Aeson..= arg1]]

instance Aeson.FromJSON VersionedTextDocumentIdentifier where
  parseJSON = Aeson.withObject "VersionedTextDocumentIdentifier" $ \arg -> VersionedTextDocumentIdentifier <$> arg Aeson..: "uri" <*> arg Aeson..: "version"
