{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentCellContentChanges where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentContentChangeEvent
import qualified Language.LSP.Protocol.Internal.Types.VersionedTextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
Content changes to a cell in a notebook document.

@since 3.18.0
@proposed
-}
data NotebookDocumentCellContentChanges = NotebookDocumentCellContentChanges 
  { {-|

  -}
  _document :: Language.LSP.Protocol.Internal.Types.VersionedTextDocumentIdentifier.VersionedTextDocumentIdentifier
  , {-|

  -}
  _changes :: [Language.LSP.Protocol.Internal.Types.TextDocumentContentChangeEvent.TextDocumentContentChangeEvent]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookDocumentCellContentChanges)

instance Aeson.ToJSON NotebookDocumentCellContentChanges where
  toJSON (NotebookDocumentCellContentChanges arg0 arg1) = Aeson.object $ concat $  [["document" Aeson..= arg0]
    ,["changes" Aeson..= arg1]]

instance Aeson.FromJSON NotebookDocumentCellContentChanges where
  parseJSON = Aeson.withObject "NotebookDocumentCellContentChanges" $ \arg -> NotebookDocumentCellContentChanges <$> arg Aeson..: "document" <*> arg Aeson..: "changes"
