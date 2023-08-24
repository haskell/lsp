{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.VersionedNotebookDocumentIdentifier where

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
A versioned notebook document identifier.

@since 3.17.0
-}
data VersionedNotebookDocumentIdentifier = VersionedNotebookDocumentIdentifier 
  { {-|
  The version number of this notebook document.
  -}
  _version :: Language.LSP.Protocol.Types.Common.Int32
  , {-|
  The notebook document's uri.
  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON VersionedNotebookDocumentIdentifier)

instance Aeson.ToJSON VersionedNotebookDocumentIdentifier where
  toJSON (VersionedNotebookDocumentIdentifier arg0 arg1) = Aeson.object $ concat $  [["version" Aeson..= arg0]
    ,["uri" Aeson..= arg1]]

instance Aeson.FromJSON VersionedNotebookDocumentIdentifier where
  parseJSON = Aeson.withObject "VersionedNotebookDocumentIdentifier" $ \arg -> VersionedNotebookDocumentIdentifier <$> arg Aeson..: "version" <*> arg Aeson..: "uri"
